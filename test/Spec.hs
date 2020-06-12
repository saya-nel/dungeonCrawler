import Test.Hspec
import qualified System.Directory as D
import qualified Data.Text as T

import GameMapSpec as GMS
import GameMap as GM
import EntiteSpec as ES
import EnvironnementSpec as ENS
import Model as M
import ModelSpec as MS
import EngineSpec as EGS

getAllMapsOf :: FilePath -> IO[String]
getAllMapsOf [] = error "directory path missing"
getAllMapsOf path = do
    allFiles <- D.getDirectoryContents path
    let allMapsFiles = filter (\filename -> T.isSuffixOf (T.pack ".txt") filename) $ T.pack <$> allFiles
    return $ (T.unpack <$> allMapsFiles)


main :: IO ()
main = do
    -- Tests GameMap
    let personalPath = "./lib/maps"
    let validMaps = "/validMaps/"
    let surrouned = "/mapSurrouned/"
    let entranceExit = "/entranceExit/"
    let doorFramed = "/doorFramed/"
    let minChests  = "/minChests/"
    let minDim = "/minDim/"
    let validCoords = "/validCoord/"
    let sinkLinked = "/sinkLinked/"

    validFilesname <- getAllMapsOf (personalPath++validMaps)
    surrFilesname <- getAllMapsOf (personalPath++surrouned)
    eeFilesname <- getAllMapsOf (personalPath++entranceExit)
    dfFilesname <- getAllMapsOf (personalPath++doorFramed)
    mcFilesname <- getAllMapsOf (personalPath++minChests)
    mdFilesname <- getAllMapsOf (personalPath++minDim)
    vcFilesname <- getAllMapsOf (personalPath++validCoords)
    slFilesname <- getAllMapsOf (personalPath++sinkLinked)

    let validMapsIO = fmap (\filename -> readFile (personalPath++validMaps++filename)) validFilesname
    let surrMapsIO = fmap (\filename -> readFile (personalPath++surrouned++filename)) surrFilesname
    let eeMapsIO = fmap (\filename -> readFile (personalPath++entranceExit++filename)) eeFilesname
    let dfMapsIO = fmap (\filename -> readFile (personalPath++doorFramed++filename)) dfFilesname
    let mcMapsIO = fmap (\filename -> readFile (personalPath++minChests++filename)) mcFilesname
    let mdMapsIO = fmap (\filename -> readFile (personalPath++minDim++filename)) mdFilesname
    let vcMapsIO = fmap (\filename -> readFile (personalPath++validCoords++filename)) vcFilesname
    let slMapsIO = fmap (\filename -> readFile (personalPath++sinkLinked++filename)) slFilesname


    validmaps <- sequence validMapsIO
    surrmaps <- sequence surrMapsIO
    eemaps <- sequence eeMapsIO
    dfmaps <- sequence dfMapsIO
    mcmaps <- sequence mcMapsIO
    mdmaps <- sequence mdMapsIO
    vcmaps <- sequence vcMapsIO
    slmaps <- sequence slMapsIO
    
    let validgamemaps = GM.gameMapRead <$> validmaps
    let surrgamemaps = GM.gameMapRead <$> surrmaps
    let eegamemaps = GM.gameMapRead <$> eemaps
    let dfgamemaps = GM.gameMapRead <$> dfmaps
    let mcgamemaps = GM.gameMapRead <$> mcmaps
    let mdgamemaps = GM.gameMapRead <$> mdmaps
    let vcgamemaps = GM.gameMapRead <$> vcmaps
    let slgamemaps = GM.gameMapRead <$> slmaps


    putStrLn $ "\n fichier de " <> validMaps
    mapM_ putStrLn validFilesname
    mapM_ hspec (GMS.validMapSpec <$> validgamemaps)

    putStrLn $ "fichiers de " <> surrouned
    mapM_ putStrLn surrFilesname
    mapM_ hspec (GMS.mapSurrounedByWallSpec <$> surrgamemaps <*> [False])

    putStrLn $ "fichiers de" <> entranceExit
    mapM_ putStrLn eeFilesname
    mapM_ hspec (GMS.entranceExitSpec <$> eegamemaps <*> [False])

    putStrLn $ "fichiers de" <> doorFramed
    mapM_ putStrLn dfFilesname
    mapM_ hspec (GMS.doorFramedSpec <$> dfgamemaps <*> [False])

    putStrLn $ "fichiers de" <> minChests
    mapM_ putStrLn mcFilesname
    mapM_ hspec (GMS.minChestsSpec <$> mcgamemaps <*> [False])

    putStrLn $ "fichiers de" <> minDim
    mapM_ putStrLn mdFilesname
    mapM_ hspec (GMS.minDimensionsSpec <$> mdgamemaps <*> [False])

    putStrLn $ "fichiers de" <> validCoords
    mapM_ putStrLn vcFilesname
    mapM_ hspec (GMS.validCoordsSpec <$> vcgamemaps <*> [False])

    putStrLn $ "fichiers de" <> sinkLinked
    mapM_ putStrLn slFilesname
    mapM_ hspec (GMS.sinkLinkedSpec <$> slgamemaps <*> [False])


    --Tests des autres modules
    let classicMapIO = readFile "./lib/maps/validMaps/realmap.txt"
    classicMap <- classicMapIO
    let  classicGameMap = GM.gameMapRead classicMap

    -- Tests Engine
    hspec $ EGS.engineSpec classicGameMap

    -- Tests Entite
    hspec ES.entiteSpec

    -- Tests Environnement
    hspec ENS.environnementSpec
  
    -- Tests Model
    hspec $ MS.modelSpec classicGameMap