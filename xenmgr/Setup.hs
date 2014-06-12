--
-- Copyright (c) 2011 Citrix Systems, Inc.
-- 
-- This program is free software; you can redistribute it and/or modify
-- it under the terms of the GNU General Public License as published by
-- the Free Software Foundation; either version 2 of the License, or
-- (at your option) any later version.
-- 
-- This program is distributed in the hope that it will be useful,
-- but WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
-- GNU General Public License for more details.
-- 
-- You should have received a copy of the GNU General Public License
-- along with this program; if not, write to the Free Software
-- Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
--

import Distribution.Simple
import Distribution.Simple.PreProcess
import Distribution.Simple.PreProcess.Unlit (unlit)
import Distribution.Package
         ( Package(..), PackageName(..) )
import Distribution.ModuleName (ModuleName)
import qualified Distribution.ModuleName as ModuleName
import Distribution.PackageDescription as PD
         ( PackageDescription(..), BuildInfo(..), Executable(..), withExe
         , Library(..), withLib, libModules )
import qualified Distribution.InstalledPackageInfo as Installed
         ( InstalledPackageInfo_(..) )
import qualified Distribution.Simple.PackageIndex as PackageIndex
import Distribution.Simple.Compiler
         ( CompilerFlavor(..), Compiler(..), compilerFlavor, compilerVersion )
import Distribution.Simple.LocalBuildInfo (LocalBuildInfo(..))
import Distribution.Simple.BuildPaths (autogenModulesDir,cppHeaderName)
import Distribution.Simple.Utils
         ( createDirectoryIfMissingVerbose, withUTF8FileContents, writeUTF8File
         , die, setupMessage, intercalate, copyFileVerbose
         , findFileWithExtension, findFileWithExtension' )
import Distribution.Simple.Program
         ( Program(..), ConfiguredProgram(..), lookupProgram, programPath
         , rawSystemProgramConf, rawSystemProgram
         , greencardProgram, cpphsProgram, hsc2hsProgram, c2hsProgram
         , happyProgram, alexProgram, haddockProgram, ghcProgram, gccProgram, ldProgram )
import Distribution.System
         ( OS(OSX, Windows), buildOS )
import Distribution.Version (Version(..))
import Distribution.Verbosity
import Distribution.Text
         ( display )

import Control.Monad (when, unless)
import Data.Maybe (fromMaybe)
import Data.List (nub)
import System.Directory (getModificationTime, doesFileExist)
import System.Info (os, arch)
import System.FilePath (splitExtension, dropExtensions, (</>), (<.>),
                        takeDirectory, normalise, replaceExtension)
import Distribution.Simple.Program (simpleProgram)

hsc2hsLdProgram = simpleProgram "hsc2hs-ld"

my_ppHsc2hs :: BuildInfo -> LocalBuildInfo -> PreProcessor
my_ppHsc2hs bi lbi = standardPP lbi hsc2hsProgram $
    [ "--cc=" ++ programPath gccProg
    , "--ld=" ++ programPath ldProg ]
--    [ "--cc=" ++ cc
--    , "--ld=" ++ ld ]

    -- Additional gcc options
 ++ [ "--cflag=" ++ opt | opt <- programArgs gccProg ]
 ++ [ "--lflag=" ++ opt | opt <- programArgs gccProg ]

    -- OSX frameworks:
 ++ [ what ++ "=-F" ++ opt
    | isOSX
    , opt <- nub (concatMap Installed.frameworkDirs pkgs)
    , what <- ["--cflag", "--lflag"] ]
 ++ [ "--lflag=" ++ arg
    | isOSX
    , opt <- PD.frameworks bi ++ concatMap Installed.frameworks pkgs
    , arg <- ["-framework", opt] ]

    -- Note that on ELF systems, wherever we use -L, we must also use -R
    -- because presumably that -L dir is not on the normal path for the
    -- system's dynamic linker. This is needed because hsc2hs works by
    -- compiling a C program and then running it.

    -- Options from the current package:
 ++ [ "--cflag="   ++ opt | opt <- hcDefines (compiler lbi) ]
 ++ [ "--cflag=-I" ++ dir | dir <- PD.includeDirs  bi ]
 ++ [ "--cflag="   ++ opt | opt <- PD.ccOptions    bi
                                ++ PD.cppOptions   bi ]
 ++ [ "--lflag=-L" ++ opt | opt <- PD.extraLibDirs bi ]
 ++ [ "--lflag=-Wl,-R," ++ opt | isELF
                          , opt <- PD.extraLibDirs bi ]
 ++ [ "--lflag=-l" ++ opt | opt <- PD.extraLibs    bi ]
 ++ [ "--lflag="   ++ opt | opt <- PD.ldOptions    bi ]

    -- Options from dependent packages
 ++ [ "--cflag=" ++ opt
    | pkg <- pkgs
    , opt <- [ "-I" ++ opt | opt <- Installed.includeDirs pkg ]
          ++ [         opt | opt <- Installed.ccOptions   pkg ] ]
 ++ [ "--lflag=" ++ opt
    | pkg <- pkgs
    , opt <- [ "-L" ++ opt | opt <- Installed.libraryDirs    pkg ]
          ++ [ "-Wl,-R," ++ opt | isELF
                           , opt <- Installed.libraryDirs    pkg ]
          ++ [ "-l" ++ opt | opt <- Installed.extraLibraries pkg ]
          ++ [         opt | opt <- Installed.ldOptions      pkg ] ]
  where
    pkgs = PackageIndex.topologicalOrder (packageHacks (installedPkgs lbi))
    Just gccProg = lookupProgram  gccProgram (withPrograms lbi)
    Just ldProg = lookupProgram hsc2hsLdProgram (withPrograms lbi)
    isOSX = case buildOS of OSX -> True; _ -> False
    isELF = case buildOS of OSX -> False; Windows -> False; _ -> True;
    packageHacks = case compilerFlavor (compiler lbi) of
      GHC -> hackRtsPackage
      _   -> id
    -- We don't link in the actual Haskell libraries of our dependencies, so
    -- the -u flags in the ldOptions of the rts package mean linking fails on
    -- OS X (it's ld is a tad stricter than gnu ld). Thus we remove the
    -- ldOptions for GHC's rts package:
    hackRtsPackage index =
      case PackageIndex.lookupPackageName index (PackageName "rts") of
        [(_, [rts])]
           -> PackageIndex.insert rts { Installed.ldOptions = [] } index
        _  -> error "No (or multiple) ghc rts package is registered!!"


-- TODO: move this into the compiler abstraction
-- FIXME: this forces GHC's crazy 4.8.2 -> 408 convention on all the other
-- compilers. Check if that's really what they want.
versionInt :: Version -> String
versionInt (Version { versionBranch = [] }) = "1"
versionInt (Version { versionBranch = [n] }) = show n
versionInt (Version { versionBranch = n1:n2:_ })
  = -- 6.8.x -> 608
    -- 6.10.x -> 610
    let s1 = show n1
        s2 = show n2
        middle = case s2 of
                 _ : _ : _ -> ""
                 _         -> "0"
    in s1 ++ middle ++ s2

standardPP :: LocalBuildInfo -> Program -> [String] -> PreProcessor
standardPP lbi prog args =
  PreProcessor {
    platformIndependent = False,
    runPreProcessor = mkSimplePreProcessor $ \inFile outFile verbosity ->
      do rawSystemProgramConf verbosity prog (withPrograms lbi)
                              (args ++ ["-o", outFile, inFile])
         -- XXX This is a nasty hack. GHC requires that hs-boot files
         -- be in the same place as the hs files, so if we put the hs
         -- file in dist/... then we need to copy the hs-boot file
         -- there too. This should probably be done another way, e.g.
         -- by preprocessing all files, with and "id" preprocessor if
         -- nothing else, so the hs-boot files automatically get copied
         -- into the right place.
         -- Possibly we should also be looking for .lhs-boot files, but
         -- I think that preprocessors only produce .hs files.
         let inBoot  = replaceExtension inFile  "hs-boot"
             outBoot = replaceExtension outFile "hs-boot"
         exists <- doesFileExist inBoot
         when exists $ copyFileVerbose verbosity inBoot outBoot
  }

hcDefines :: Compiler -> [String]
hcDefines comp =
  case compilerFlavor comp of
    GHC  -> ["-D__GLASGOW_HASKELL__=" ++ versionInt version]
    JHC  -> ["-D__JHC__=" ++ versionInt version]
    NHC  -> ["-D__NHC__=" ++ versionInt version]
    Hugs -> ["-D__HUGS__"]
    _    -> []
  where version = compilerVersion comp

main = defaultMainWithHooks $ simpleUserHooks {
         hookedPreProcessors = [ ("hsc", my_ppHsc2hs) ]
       , hookedPrograms = hsc2hsLdProgram : hookedPrograms simpleUserHooks
       }
