{-
 - BananaTest.hs
 - By Steven Smith
 -}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import PlyParser hiding (main)
import GLMath
import Data.Array.Unboxed
import Data.Array.ST
import System.Environment
import Text.ParserCombinators.Parsec hiding (State, getState)
import Graphics.Rendering.OpenGL
import Graphics.UI.GLUT
import Control.Applicative
import Control.Monad
import Control.Monad.ST
import System.Exit
import Reactive.Banana

getValues :: String -> [(String, [[Float]])] -> Maybe [[Float]]
getValues s [] = Nothing
getValues s1 ((s2, fs):next) | s1 == s2 = Just fs
                             | otherwise = getValues s1 next

listToArray :: [[Float]] -> Array (Int,Int) Float
listToArray fs = array ((0,0),(h, w))
    [((i,j),fs!!i!!j) | i <- [0..h], j <- [0..w]]
    where h = length fs - 1
          w = (length $ fs !! 0) - 1

listToVertices :: [[Float]] -> Array Int (Vertex3 GLfloat)
listToVertices fs = array (0,n)
    [(i,f vs) | (i,vs) <- zip [0..] fs]
    where n = length fs - 1
          f (x:y:z:_) = Vertex3 x y z

listToIndices :: [[Float]] -> Array Int (Vector3 Int)
listToIndices fs = array (0,n)
    [(i,f vs) | (i,vs) <- zip [0..] fs]
    where n = length fs - 1
          f (x:y:z:_) = Vector3 (floor x) (floor y) (floor z)

printVertices = putStrLn . maybe "No vertices found!" (show . listToVertices)

printIndices  = putStrLn . maybe "No indices found!" (show . listToIndices)

printVerticesAndIndices result = do printVertices (getValues "vertex" result)
                                    printIndices  (getValues "face" result)

computeFaceNormals :: Array Int (Vertex3 GLfloat)
                   -> Array Int (Vector3 Int)
                   -> Array Int (Normal3 GLfloat)
computeFaceNormals vs fs = array b [(i, f vs fs i) | i <- [l..u]]
    where b@(l,u)   = bounds fs
          f vs fs i = let Vector3 a b c = fs ! i
                          left  = (vs ! c) - (vs ! a)
                          right = (vs ! b) - (vs ! a)
                       in toNormal3 $ left `cross` right

getFaceNormals :: [(String, [[Float]])] -> Maybe (Array Int (Normal3 GLfloat))
getFaceNormals result = do verts <- getValues "vertex" result
                           faces <- getValues "face"   result
                           let v =  listToVertices verts
                               f =  listToIndices  faces
                           return $ computeFaceNormals v f

printFaceNormals :: [(String, [[Float]])] -> IO ()
printFaceNormals = putStrLn . maybe "No face normals!" show . getFaceNormals

computeVertexNormals :: Array Int (Normal3 GLfloat)
                     -> Array Int (Vector3 Int)
                     -> Array Int (Vertex3 GLfloat)
                     -> Array Int (Normal3 GLfloat)
computeVertexNormals fns fs vs = runSTArray $ do
    arr <- newArray (bounds vs) (Normal3 0 0 0)
    let update f i = liftM f (readArray arr i) >>= (\e -> writeArray arr i e)
    flip mapM (zip (elems fs) (elems fns)) $ \(f,n) -> let Vector3 a b c = f
        in mapM (update (+ n)) [a,b,c]
    return arr

-- Drawing stuff

data State = State {
    verticesS   :: Array Int (Vertex3 GLfloat),
    facesS      :: Array Int (Vector3 Int),
    faceNormals :: Array Int (Normal3 GLfloat),
    vertNormals :: Array Int (Normal3 GLfloat),
    transVec    :: Vector3 GLfloat,
    scaleVec    :: Vector3 GLfloat
    }
    deriving (Show)

par2 :: (a -> a -> a) -> (a -> a -> a) -> (a, a) -> a -> (a, a)
par2 fn1 fn2 (a,b) n = (fn1 a n, fn2 b n)

minmax :: (Ord a, Bounded a) => [a] -> (a,a)
minmax = foldl (par2 min max) (maxBound, minBound)

vec2vert (Vector3 x y z) = Vertex3 x y z
vert2vec (Vertex3 x y z) = Vector3 x y z

getState :: [(String, [[Float]])] -> Maybe State
getState result = do verts <- getValues "vertex" result
                     faces <- getValues "face"   result
                     let v =  listToVertices verts
                         f =  listToIndices  faces
                         n =  computeFaceNormals v f
                         vn = computeVertexNormals n f v
                         (minV, maxV) = foldl (par2 (liftA2 min) (liftA2 max))
                                        (pure (1e10), pure (-1e10)) (elems v)
                     return $ State {
                        verticesS   = v,
                        facesS      = f,
                        faceNormals = n,
                        vertNormals = vn,
                        transVec    = vert2vec $ (minV + maxV) / pure (-2),
                        scaleVec    = vert2vec $ pure 25 / (maxV - minV)
                     }

drawTriangle :: (Normal3 GLfloat, Normal3 GLfloat, Normal3 GLfloat)
             -> (Vertex3 GLfloat, Vertex3 GLfloat, Vertex3 GLfloat)
             -> IO ()
drawTriangle (n1, n2, n3) (v1, v2, v3) = do
    normal n1
    vertex v1
    normal n2
    vertex v2
    normal n3
    vertex v3

getTriangle :: Vector3 Int
            -> Array Int (Normal3 GLfloat)
            -> Array Int (Vertex3 GLfloat)
            -> ( (Normal3 GLfloat, Normal3 GLfloat, Normal3 GLfloat)
               , (Vertex3 GLfloat, Vertex3 GLfloat, Vertex3 GLfloat) )
getTriangle f ns vs = ( (ns ! f1, ns ! f2, ns ! f3)
                       , (vs ! f1, vs ! f2, vs ! f3) )
    where Vector3 f1 f2 f3 = f

drawModel :: State -> IO ()
drawModel s = let faces = facesS s
                  verts = verticesS s
                  scal = scaleVec s
                  trans = transVec s
                  norms = vertNormals s
               in do
                  preservingMatrix $ do
                      loadIdentity
                      let Vector3 sx sy sz = scal
                      scale sx sy sz
                      translate trans
                      let draw f = uncurry drawTriangle $ getTriangle f norms verts
                      renderPrimitive Triangles $ mapM_ draw $ elems faces

-- GLUT stuff

reshape :: ReshapeCallback
reshape size@(Size w h) = do
    postRedisplay Nothing

display :: State -> Vector3 GLfloat -> Size -> GLdouble -> DisplayCallback
display state pos size aspect = do
    clear [ ColorBuffer, DepthBuffer ]
    matrixMode $= Projection
    let l = -50 :: GLdouble
        r =  50 :: GLdouble
        Size w h = size
        Vector3 sx sy sz = scaleVec state
    viewport $= (Position 0 0, size)
    loadIdentity
    if w <= h
    then ortho l r (l / aspect) (r / aspect) (-100) 100
    else ortho (l * aspect) (r * aspect) l r (-100) 100
    translate pos

    matrixMode $= Modelview 0
    loadIdentity
    drawModel state
    swapBuffers

timer :: TimerCallback -> TimerCallback
timer next = do
    postRedisplay Nothing
    addTimerCallback (1000 `div` 30) next

toggle Disabled = Enabled
toggle Enabled  = Disabled

keyboard :: State -> KeyboardMouseCallback
keyboard state key keyState mods _ = do
    case (key, keyState) of
        (Char 'q', Down) -> exitWith ExitSuccess
        (Char '\27', Down) -> exitWith ExitSuccess
        (Char '1', Down) -> light (Light 0) $~ toggle
        (Char '2', Down) -> light (Light 1) $~ toggle
        (Char '3', Down) -> light (Light 2) $~ toggle
        otherwise -> return ()
    postRedisplay Nothing

setLights = let black = Color4 0 0 0 1
                red   = Color4 1 0 0 1
                green = Color4 0 1 0 1
                blue  = Color4 0 0 1 1
                rloc  = Vertex4    0    20  100 1
                gloc  = Vertex4 (-20) (-10) 100 1
                bloc  = Vertex4   20  (-10) 100 1
                rdir  = Normal3    0  (-20) (-100)
                gdir  = Normal3   20    10  (-100)
                bdir  = Normal3 (-20)   10  (-100)
             in do
                ambient       (Light 0) $= black
                diffuse       (Light 0) $= red
                specular      (Light 0) $= red
                position      (Light 0) $= rloc
                spotDirection (Light 0) $= rdir

                ambient       (Light 1) $= black
                diffuse       (Light 1) $= green
                specular      (Light 1) $= green
                position      (Light 1) $= gloc
                spotDirection (Light 1) $= gdir

                ambient       (Light 2) $= black
                diffuse       (Light 2) $= blue
                specular      (Light 2) $= blue
                position      (Light 2) $= bloc
                spotDirection (Light 2) $= bdir

                light (Light 0) $= Enabled
                light (Light 1) $= Enabled
                light (Light 2) $= Enabled

-- Event source stuff

type EventSource a = (AddHandler a, a -> IO ())

addHandler :: EventSource a -> AddHandler a
addHandler = fst

fire :: EventSource a -> a -> IO ()
fire = snd

makeSources = (,,,) <$> newAddHandler <*> newAddHandler
                    <*> newAddHandler <*> newAddHandler

setupEvents state = do
    sources <- makeSources
    network <- compile $ setupNetwork state sources
    actuate network
    let (eDisplay, eReshape, eTimer, eKeyboard) = sources
    displayCallback $= fire eDisplay ()
    reshapeCallback $= Just (fire eReshape)
    keyboardMouseCallback $= Just (\a b c d -> fire eKeyboard (a,b,c,d))
    fire eTimer ()

-- Functions used by events to do stuff

move (Char 'w', Down, _, _) = (+ (Vector3   0 (-1)  0 ))
move (Char 'a', Down, _, _) = (+ (Vector3 (-1)  0   0 ))
move (Char 's', Down, _, _) = (+ (Vector3   0   1   0 ))
move (Char 'd', Down, _, _) = (+ (Vector3   1   0   0 ))
move (Char 'w', Up,   _, _) = (+ (Vector3   0   1   0 ))
move (Char 'a', Up,   _, _) = (+ (Vector3   1   0   0 ))
move (Char 's', Up,   _, _) = (+ (Vector3   0 (-1)  0 ))
move (Char 'd', Up,   _, _) = (+ (Vector3 (-1)  0   0 ))
move _ = id

isKey (Char _, _, _, _) = True
isKey _                 = False

setupNetwork :: forall t. State
             -> (EventSource (),
                EventSource Size,
                EventSource (),
                EventSource (Key, KeyState, Modifiers, Position))
             -> NetworkDescription t ()
setupNetwork state (d, r, t, k) = do
    eDisplay  <- fromAddHandler $ addHandler d
    eReshape  <- fromAddHandler $ addHandler r
    eTimer    <- fromAddHandler $ addHandler t
    eKeyboard <- fromAddHandler $ addHandler k
    let bPosition :: Behavior t (Vector3 GLfloat)
        bPosition = accumB (pure 0) $ (+) <$> bVelocity <@ eTimer

        bVelocity :: Behavior t (Vector3 GLfloat)
        bVelocity = accumB (pure 0) $ move <$> eKey

        bWindowSize :: Behavior t Size
        bWindowSize = stepper (Size 500 500) eReshape

        bAspect :: Behavior t GLdouble
        bAspect = (\(Size w h) -> fromIntegral w / fromIntegral h) <$> bWindowSize

        eKey :: Event t (Key, KeyState, Modifiers, Position)
        eKey = filterE isKey eKeyboard

    reactimate $ timer (fire t ()) <$ eTimer
    reactimate $ display state <$> bPosition <*> bWindowSize <*> bAspect <@ eDisplay
    reactimate $ reshape <$> eReshape

setGL :: State -> IO ()
setGL state = do
    getArgsAndInitialize
    initialDisplayMode $= [ RGBMode, WithDepthBuffer, DoubleBuffered ]
    initialWindowPosition $= Position 80 80
    initialWindowSize $= Size 500 500
    createWindow "Ply Model Renderer"
    shadeModel $= Smooth
    lighting $= Enabled
    normalize $= Enabled
    globalKeyRepeat $= GlobalKeyRepeatOff
    setLights
    depthFunc $= Just Less
    colorMaterial $= Just (Front, Diffuse)
    materialDiffuse Front $= Color4 1 1 1 1
    materialAmbient Front $= Color4 0.3 0.3 0.3 1
    materialShininess Front $= 16
    setupEvents state
    mainLoop

main = do
    args <- getArgs
    case args of
        []       -> putStrLn "No file given."
        (file:_) -> parseFromFile plyParser file >>= (\x -> case x of
                        Left err -> print err
                        Right result -> maybe (return ()) setGL $ getState result)
