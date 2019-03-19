module Main where

import AI.AIFH.Utils

import Control.Arrow
import Control.Monad
import Graphics.UI.Gtk
import Graphics.UI.Gtk.General.CssProvider
import Graphics.UI.Gtk.General.StyleContext


import Data.Ord
import Data.List
import Data.IORef
import Foreign (allocaArray)
import Graphics.Rendering.Cairo hiding (x,y)
import Graphics.Rendering.Cairo.Types (PixelData)
import Foreign.Storable (Storable(..))
import Foreign.C (CUChar)

import qualified Data.Vector.Unboxed as VU

-- * Constants

-- | Number of channels pixel
chan :: Int
chan = 4 -- rgb + alpha

-- | Size of data after down sampling
sampleSize :: (Int,Int)
sampleSize = (5,7)


-- * Types

-- | The UI structure, populated from the glade definition
data GUI = GUI {
      gWindow :: Window,
      gDeleteLetter :: Button,
      gLoadList :: Button,
      gSaveList ::Button,
      gAddLetter :: Button,
      gRecognizeLetter :: Button,
      gClear :: Button,
      gSample :: Button,
      gDrawingArea :: DrawingArea,
      gSampleArea :: DrawingArea,
      gLetterListView :: TreeView,
      gLetterListStore :: ListStore Letter
}

-- | Drawing data: the pixel data and size dimenensions
data DrawingData = DrawingData
    { ddPixels :: PixelData
    , ddWidth  :: Int
    , ddHeight :: Int
    , ddStride :: Int
    }

-- | A letter: the letter itself, and the sample data as a vector
data Letter = Letter
    { lName :: Char
    , lData :: VU.Vector Double
    } deriving (Show,Read,Eq,Ord)

-- * Data manipulation

-- | Crop a sparse array of coordinates
-- return the cropped array and the dimensions
crop :: [(Int,Int)] -> ([(Int,Int)],(Int,Int))
crop ps = let
    minx  = minimum $ map fst ps
    maxx = maximum $ map fst ps
    miny  = minimum $ map snd ps
    maxy = maximum $ map snd ps
    in (map (\(a,b)->(a-minx,b-miny)) ps,(maxx-minx,maxy-miny))

-- | Sample all coordinates
sampleAll :: [(Int,Int)] -> [(Int,Int)]
sampleAll = downSample (5,7) . crop . addNeighbours 1

-- | Downsample to a specific size a cropped array
downSample :: (Int,Int) -> ([(Int,Int)],(Int,Int)) -> [(Int,Int)]
downSample (sampW,sampH) (ps,(origW,origH)) = let
    ratiox = (fromIntegral sampW - 1) / fromIntegral origW
    ratioy = (fromIntegral sampH - 1) / fromIntegral origH
    in ordNub $ map (\(a,b)-> (sample a ratiox,sample b ratioy)) ps
    where
        sample :: Int -> Double -> Int
        sample a r = round ((fromIntegral a) * r)

-- | Recognize a letter from sparse coordinates
recognize :: [Letter] -> [(Int,Int)] -> Maybe Letter
recognize [] _ = Nothing
recognize _ [] = Nothing
recognize letters pos = Just $ fst $ minimumBy (comparing snd) $ map (dist $ sparseToVector pos) letters
    where
          -- get the distance between the given sample and the letter data
          dist po letter = (letter,euclideanV (lData letter) po)

-- | convert sparse coordinates to a vector or 0 (empty) and 1 (pixel drawn)
sparseToVector :: [(Int,Int)] -> VU.Vector Double
sparseToVector po =
   let
     sampleV = VU.replicate sampleLength 0
     sampleLength = fst sampleSize * snd sampleSize
   in sampleV VU.// map (\ (x, y) -> (x + y * (fst sampleSize), 1)) po


-- | Get all neighbours of a coordinates, within a specified range
addNeighbours :: Int -> [(Int,Int)] -> [(Int,Int)]
addNeighbours sz = concatMap (neighbours sz)

-- | Get all neighbours of a given coordinate, within a specified range
neighbours :: Int -> (Int,Int) -> [(Int,Int)]
neighbours sz (a,b) = [(a+x,b+y) | x<-[-sz..sz],y<-[-sz..sz]]

-- * UI

-- | Main entry point
main :: IO ()
main = do
    initGUI
    gui <-buildGUI

    Requisition w h <- widgetSizeRequest (gDrawingArea gui)
    Requisition sampW sampH <- widgetSizeRequest (gSampleArea gui)

    let stride = w * chan
    -- allocate arrays for drawing
    allocaArray (stride * h) $ \ pbData ->
        allocaArray (stride * sampH) $ \ sampData -> do
            let dd = DrawingData pbData w h stride
            let sampDd = DrawingData sampData sampW sampH stride
            updateAll dd (255,255,255)
            updateAll sampDd (255,255,255)
            addEvents gui dd sampDd
            widgetShowAll $ gWindow gui
            mainGUI

-- build the UI from the glade definition
buildGUI :: IO GUI
buildGUI = do
   bld <- builderNew
   builderAddFromFile bld "ui/ocr.xml"

   css <- cssProviderGetDefault
   cssProviderLoadFromPath css "ui/ocr.css"

       -- Load main window
   window <- builderGetObject bld castToWindow "ocrWindow"
   windowSetPosition window WinPosCenter

   Just screen <- screenGetDefault
   styleContextAddProviderForScreen screen css 1
   [del,load,savel,addl,recl,clear,sample]<-mapM (builderGetObject bld castToButton)
        ["deleteListButton","loadListButton","saveListButton","letterAddButton"
        ,"letterRecognizeButton","clearButton","sampleButton"]
   [dr,sam] <- mapM (builderGetObject bld castToDrawingArea) ["drawingArea","sampleArea"]

   letterListView <- builderGetObject bld castToTreeView "treeview1"
   treeViewSetHeadersVisible letterListView False
   letterList <- listStoreNew []
   addTextColumn letterListView letterList ((:"") . lName)
   treeViewSetModel letterListView letterList
   sel <- treeViewGetSelection letterListView
   treeSelectionSetMode sel SelectionMultiple
   return $ GUI window del load savel addl recl clear sample dr sam letterListView letterList
    where
        addTextColumn view model f =
            do
            col <- treeViewColumnNew
            rend <- cellRendererTextNew
            treeViewColumnPackStart col rend True
            cellLayoutSetAttributes col rend model (\row -> [ cellText := f row ])

            treeViewColumnSetExpand col True
            treeViewAppendColumn view col

-- | Add all events to the GUI components
addEvents :: GUI -> DrawingData -> DrawingData -> IO()
addEvents gui dData sampDData= do
    gWindow gui `on` deleteEvent $ liftIO mainQuit >> return False
    widgetAddEvents (gDrawingArea gui) [Button1MotionMask,PointerMotionHintMask]
    -- drawn coordinates
    coords <- newIORef []
    -- sample coordinates
    sampleCoords <- newIORef []
    -- press button on drawing area: start drawing
    gDrawingArea gui `on` buttonPressEvent $ do
        newPos <- eventCoordinates
        liftIO $ modifyIORef coords (\ps->ps++[roundT newPos])
        return True
    -- listen to mouse motion while drawing
    gDrawingArea gui `on` motionNotifyEvent $ do
        newPos <- eventCoordinates
        when (inside dData newPos) $
            liftIO $ do
                modifyIORef coords (addCoord newPos)
                widgetQueueDraw (gDrawingArea gui)
        eventRequestMotions
        return True
    -- paint drawing area
    gDrawingArea gui `on` draw $ do
        liftIO $ do
            pos <- readIORef coords
            -- use several pixels per coordinates
            mapM_ (updatePixel dData (0,0,0)) $ addNeighbours 1 pos
        updateCanvas dData
    -- clear drawing area
    gClear gui `on` buttonPressEvent $ do
        liftIO $ do
            writeIORef coords []
            updateAll dData (255,255,255)
            widgetQueueDraw (gDrawingArea gui)
        return True
    -- calculate sample and draw it on sample area
    gSample gui `on` buttonPressEvent $ do
        liftIO $ do
            updateAll sampDData (255,255,255)
            widgetQueueDraw (gSampleArea gui)
            pos <- readIORef coords
            -- use several pixel per coordinate
            let sample = map (join (***) (+3) . join (***) (*10)) $ sampleAll pos
            writeIORef sampleCoords sample
            widgetQueueDraw (gSampleArea gui)
        return True
    -- actually paint sample area
    gSampleArea gui `on` draw $ do
        liftIO $ do
            pos <- readIORef sampleCoords
            mapM_ (updatePixel sampDData (0,0,0)) $ addNeighbours 3 pos
        --    print pos
        updateCanvas sampDData
    -- add a letter to the list
    gAddLetter gui `on` buttonPressEvent $ do
        liftIO $ do
            l <- openLetterDialog (gWindow gui)
            case l of
                Just c -> do
                    pos <- readIORef coords
                    let sample = sparseToVector $ sampleAll pos
                    void $ listStoreAppend (gLetterListStore gui) (Letter c sample)
                _ -> return ()
        return True
    -- delete a letter from the list
    gDeleteLetter gui `on` buttonPressEvent $ do
        liftIO $ do
            sel <- treeViewGetSelection (gLetterListView gui)
            tps <- treeSelectionGetSelectedRows sel
            mapM_ (listStoreRemove (gLetterListStore gui) . head) tps
            return ()
        return True
    -- save letter list to file
    gSaveList gui `on` buttonPressEvent $ do
        liftIO $ do
            vals <- listStoreToList (gLetterListStore gui)
            saveList vals (gWindow gui)
            return ()
        return True
    -- load letter list from file
    gLoadList gui `on` buttonPressEvent $ do
        liftIO $ do
            mlist <- loadList (gWindow gui)
            case mlist of
                Just list -> do
                    listStoreClear (gLetterListStore gui)
                    mapM_ (listStoreAppend (gLetterListStore gui)) list
                Nothing -> return ()
        return True
    -- recognize a letter
    gRecognizeLetter gui `on` buttonPressEvent $ do
        liftIO $ do
            pos <- readIORef coords
            let sample = sampleAll pos
            vals <- listStoreToList (gLetterListStore gui)
            openRecognizeDialog (gWindow gui) (recognize vals sample)
        return True
    return ()

-- | open a dialog to ask the user for the letter she drew
-- return the character or Nothing if the dialog was cancelled
openLetterDialog :: Window -> IO (Maybe Char)
openLetterDialog w = do
    d <- dialogNew
    set d [windowTransientFor := w]
    dialogAddButton d "_Cancel" ResponseCancel
    dialogAddButton d "_OK" ResponseOk
    area <- castToBox <$> dialogGetContentArea d
    lbl <- labelNew $ Just "Letter?"
    txt <- entryNew
    entrySetMaxLength txt 1
    containerAdd area lbl
    containerAdd area txt
    widgetShowAll area
    rid <- dialogRun d
    widgetHide d
    case rid of
        ResponseOk -> do
            letter <- entryGetText txt
            case length letter of
                1 -> return $ Just $ head letter
                _ -> return Nothing
        _ -> return Nothing

-- | Say in a dialog if a letter was recognized or not
openRecognizeDialog :: Window -> Maybe Letter -> IO()
openRecognizeDialog w ml = do
    d <- dialogNew
    set d [windowTransientFor := w]
    dialogAddButton d "_Close" ResponseClose
    area <- castToBox <$> dialogGetContentArea d
    lbl <- labelNew $ Just $ case ml of
            Just l  -> "Recognized letter:" ++ show (lName l)
            Nothing -> "No letter recognized!"
    containerAdd area lbl
    widgetShowAll area
    dialogRun d
    widgetHide d

-- | Ask for a file to save the list to
saveList :: [Letter] -> Window -> IO ()
saveList letters w = do
    d <- fileChooserDialogNew (Just "Save to file") (Just w)
            FileChooserActionSave [("_Cancel",ResponseCancel),("_OK",ResponseOk)]
    rid <- dialogRun d
    widgetHide d
    case rid of
        ResponseOk -> do
            mf <-fileChooserGetFilename d
            case mf of
                Just f -> writeFile f (show letters)
                _ -> return ()
        _ -> return ()

-- | Ask from the file to load the list from
-- return Nothing if the dialog was cancelled
loadList :: Window -> IO (Maybe [Letter])
loadList w = do
    d <- fileChooserDialogNew (Just "Load from file") (Just w)
            FileChooserActionOpen [("_Cancel",ResponseCancel),("_OK",ResponseOk)]
    rid <- dialogRun d
    widgetHide d
    case rid of
        ResponseOk -> do
            mf <-fileChooserGetFilename d
            case mf of
                Just f -> Just .read <$> readFile f
                _ -> return Nothing
        _ -> return Nothing

-- | round a tuple of Doubles
roundT :: (Double,Double) -> (Int,Int)
roundT = join (***) round

-- | add a coordinate to a list, rounding it and calculating path to previous
addCoord :: (Double,Double) -> [(Int,Int)] ->  [(Int,Int)]
addCoord newPos [] = [roundT newPos]
addCoord newPos ps = --ps ++ [roundT newPos]
    ps ++ tail (fillList (last ps : [roundT newPos]))

-- | This is probably a hack. The mouse event only gives us some coordinates but not all
-- so we fill the coordinates between the previous one and the new one
fillList :: [(Int,Int)] -> [(Int,Int)]
fillList cs =  concat $ scanl fill' [] cs
  where
    fill' [] p = [p]
    fill' ps e@(ex,ey) = let
        l@(lx,ly) = last ps
        in if e==l
                then [(ex,ey)]
                else (lx,ly) : fill' [nearest l e] (ex,ey)
    dis :: (Int,Int) -> (Int,Int) -> Double
    dis (a,b) (c,d)=euclidean [fromIntegral a,fromIntegral b] [fromIntegral c,fromIntegral d]
    nearest p1 p2 = fst $ minimumBy (comparing snd)
                            (map (\ p -> (p, dis p p2)) $ neighbours 1 p1)

-- | update a given pixel with the given color
updatePixel :: DrawingData -> ( CUChar, CUChar, CUChar)  -> (Int,Int) -> IO()
updatePixel (DrawingData pb w h stride) (r,g,b) (x,y) =
    when (x>=0 && y>=0 && x<w && y<h) $ do
        pokeByteOff pb (2+x*chan+y*stride) r
        pokeByteOff pb (1+x*chan+y*stride) g
        pokeByteOff pb (0+x*chan+y*stride) b

-- | update all pixels with the given color
updateAll :: DrawingData -> ( CUChar, CUChar, CUChar) -> IO()
updateAll dd@(DrawingData _ w h _) color  = do
    let coords = [(x,y) | x <-[0..w-1] , y <- [0..h]]
    mapM_ (updatePixel dd color) coords

-- | update the canvas
updateCanvas :: DrawingData -> Render ()
updateCanvas (DrawingData pb w h stride) = do
  s <- liftIO $ createImageSurfaceForData pb FormatRGB24 w h stride
  setSourceSurface s 0 0
  paint

-- | is a point inside a drawing area?
inside :: DrawingData -> (Double,Double) -> Bool
inside (DrawingData _ w h _) (x,y) = x>=0 && y>=0 && x<=fromIntegral w && y<=fromIntegral h


