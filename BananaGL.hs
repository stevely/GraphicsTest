{-
 - BananaGL.hs
 - By Steven Smith
 -}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import GLMath

import Control.Applicative
import Control.Monad
import Graphics.Rendering.OpenGL
import Graphics.UI.GLUT
import Reactive.Banana

-- Drawing stuff

drawFace :: Normal3 GLfloat
         -> (Vertex3 GLfloat, Vertex3 GLfloat, Vertex3 GLfloat, Vertex3 GLfloat)
         -> IO ()
drawFace norm (a, b, c, d) = do
    normal norm
    vertex a
    vertex b
    vertex c
    vertex d

drawCube :: IO ()
drawCube = let size = 20
               indices = [Vertex3 (-size) (-size) (-size),
                          Vertex3 (-size) (-size) ( size),
                          Vertex3 (-size) ( size) ( size),
                          Vertex3 (-size) ( size) (-size),
                          Vertex3 ( size) ( size) (-size),
                          Vertex3 ( size) (-size) (-size),
                          Vertex3 ( size) (-size) ( size),
                          Vertex3 ( size) ( size) ( size)]
               pick a b c d = (f a, f b, f c, f d)
                    where f = (indices !!)
               faces = [pick 0 1 2 3,
                        pick 7 6 5 4,
                        pick 5 6 1 0,
                        pick 7 4 3 2,
                        pick 1 2 7 6,
                        pick 3 4 5 0]
               normals = [Normal3 (-1)   0   0,
                          Normal3   1    0   0,
                          Normal3   0  (-1)  0,
                          Normal3   0    1   0,
                          Normal3   0    0   1,
                          Normal3   0    0 (-1)]
            in renderPrimitive Quads $ zipWithM_ drawFace normals faces

-- GLUT stuff

reshape :: ReshapeCallback
reshape size@(Size w h) = do
    viewport $= (Position 0 0, size)
    postRedisplay Nothing

display :: Vector3 GLfloat -> GLdouble -> (GLfloat, GLfloat) -> DisplayCallback
display pos aspect (xrot, yrot) = do
    clear [ ColorBuffer, DepthBuffer ]

    matrixMode $= Projection
    loadIdentity
    perspective 60 aspect 5 505

    matrixMode $= Color
    loadIdentity
    rotate yrot $ Vector3 1 0 0
    rotate xrot $ Vector3 0 1 0
    translate $ negate pos

    matrixMode $= Modelview 0
    loadIdentity
    rotate yrot $ Vector3 1 0 0
    rotate xrot $ Vector3 0 1 0
    translate $ negate pos

    drawCube

    swapBuffers

timer :: TimerCallback -> TimerCallback
timer next = do
    postRedisplay Nothing
    addTimerCallback (1000 `div` 30) next

setLights = let white = Color4 1 1 1 1
             in do
                lighting           $= Enabled
                light    (Light 0) $= Enabled
                ambient  (Light 0) $= white
                diffuse  (Light 0) $= white
                specular (Light 0) $= white

-- Event source stuff

type EventSource a = (AddHandler a, a -> IO ())

addHandler :: EventSource a -> AddHandler a
addHandler = fst

fire :: EventSource a -> a -> IO ()
fire = snd

makeSources = (,,,,) <$> newAddHandler <*> newAddHandler
                     <*> newAddHandler <*> newAddHandler
                     <*> newAddHandler

setupEvents = do
    sources <- makeSources
    network <- compile $ setupNetwork sources
    actuate network
    let (eDisplay, eReshape, eTimer, eKeyboard, eMouse) = sources
    displayCallback $= fire eDisplay ()
    reshapeCallback $= Just (fire eReshape)
    keyboardMouseCallback $= Just (\a b c d -> fire eKeyboard (a,b,c,d))
    motionCallback $= Just (fire eMouse)
    fire eTimer ()

-- Functions used by events to do stuff

move (Char 'w', Down, _, _) = (+ (Vector3   0    0  (-1) ))
move (Char 'a', Down, _, _) = (+ (Vector3 (-1)   0    0  ))
move (Char 's', Down, _, _) = (+ (Vector3   0    0    1  ))
move (Char 'd', Down, _, _) = (+ (Vector3   1    0    0  ))
move (Char 'w', Up,   _, _) = (+ (Vector3   0    0    1  ))
move (Char 'a', Up,   _, _) = (+ (Vector3   1    0    0  ))
move (Char 's', Up,   _, _) = (+ (Vector3   0    0  (-1) ))
move (Char 'd', Up,   _, _) = (+ (Vector3 (-1)   0    0  ))
move _ = id

isKey (Char _, _, _, _) = True
isKey _                 = False

isMousedown (MouseButton LeftButton, Down, _, _) = True
isMousedown _                                    = False

isMouseup (MouseButton LeftButton, Up, _, _) = True
isMouseup _                                    = False

posDiff :: Position -> Position -> Position
posDiff (Position x1 y1) (Position x2 y2) = Position (x1-x2) (y1-y2)

posSum :: Position -> Position -> Position
posSum (Position x1 y1) (Position x2 y2) = Position (x1+x2) (y1+y2)

scalePos :: Position -> GLfloat -> (GLfloat, GLfloat)
scalePos (Position x y) scale = (fromIntegral x * scale, fromIntegral y * scale)

moveRotated :: GLfloat -> Vector3 GLfloat -> Vector3 GLfloat
moveRotated rot (Vector3 x y z) = let radRot = rot / 180 * pi
                                      sinf = sin radRot
                                      cosf = cos radRot
                                   in Vector3 (x * cosf - z * sinf)
                                              y
                                              (z * cosf + x * sinf)

normalizeIfVector :: Vector3 GLfloat -> Vector3 GLfloat
normalizeIfVector v@(Vector3 0 0 0) = v
normalizeIfVector v                 = normalizeV v

-- Network stuff

setupNetwork :: forall t.
                (EventSource (),
                EventSource Size,
                EventSource (),
                EventSource (Key, KeyState, Modifiers, Position),
                EventSource Position)
             -> NetworkDescription t ()
setupNetwork (d, r, t, k, m) = do
    eDisplay  <- fromAddHandler $ addHandler d
    eReshape  <- fromAddHandler $ addHandler r
    eTimer    <- fromAddHandler $ addHandler t
    eKeyboard <- fromAddHandler $ addHandler k
    eMouse    <- fromAddHandler $ addHandler m
    let bPosition :: Behavior t (Vector3 GLfloat)
        bPosition = accumB (Vector3 0 0 100) $ (+) . normalizeIfVector <$>
            (moveRotated . fst <$> bLookAt <*> bVelocity) <@ eTimer

        bVelocity :: Behavior t (Vector3 GLfloat)
        bVelocity = accumB (pure 0) $ move <$> eKey

        eMouseStart :: Event t ((Position, Position) -> (Position, Position))
        eMouseStart = (\p _ -> (p,p)) <$> eMouseDown

        eMouseMove :: Event t ((Position, Position) -> (Position, Position))
        eMouseMove = (\p (p1,p2) -> (p,p1)) <$> eMouse

        eMouseEnd :: Event t ((Position, Position) -> (Position, Position))
        eMouseEnd = (\p _ -> (p,p)) <$> eMouseUp

        zPos = Position 0 0

        eMouseEvents :: Event t (Position, Position)
        eMouseEvents = accumE (zPos, zPos) $ eMouseStart `union`
                                             eMouseMove  `union`
                                             eMouseEnd

        bLookAt :: Behavior t (GLfloat, GLfloat)
        bLookAt = flip scalePos <$>
            ((\(Size w _) -> 60 / fromIntegral w) <$> bWindowSize) <*>
            bLookAt'

        bLookAt' :: Behavior t Position
        bLookAt' = accumB zPos $ posSum <$> (uncurry posDiff <$> eMouseEvents)

        bWindowSize :: Behavior t Size
        bWindowSize = stepper (Size 500 500) eReshape

        bAspect :: Behavior t GLdouble
        bAspect = (\(Size w h) -> fromIntegral w / fromIntegral h) <$> bWindowSize

        eKey :: Event t (Key, KeyState, Modifiers, Position)
        eKey = filterE isKey eKeyboard

        eMouseDown :: Event t Position
        eMouseDown = (\(_, _, _, p) -> p) <$> filterE isMousedown eKeyboard

        eMouseUp :: Event t Position
        eMouseUp = (\(_, _, _, p) -> p) <$> filterE isMouseup eKeyboard

    reactimate $ timer (fire t ()) <$ eTimer
    reactimate $ display <$> bPosition <*> bAspect <*> bLookAt <@ eDisplay
    reactimate $ reshape <$> eReshape

main = do
    getArgsAndInitialize
    initialDisplayMode $= [ RGBMode, WithDepthBuffer, DoubleBuffered ]
    initialWindowPosition $= Position 80 80
    initialWindowSize $= Size 500 500
    createWindow "Banana and OpenGL Demo"
    shadeModel $= Smooth
    globalKeyRepeat $= GlobalKeyRepeatOff
    setLights
    depthFunc $= Just Less
    colorMaterial $= Just (Front, Diffuse)
    materialDiffuse Front $= Color4 1 1 1 1
    materialAmbient Front $= Color4 0.3 0.3 0.3 1
    materialShininess Front $= 16
    setupEvents
    mainLoop
