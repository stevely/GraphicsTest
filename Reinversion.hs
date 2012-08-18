{-
 - Reinversion.hs
 - Code based on code found at the following URL:
 - http://blog.sigfpe.com/2011/10/quick-and-dirty-reinversion-of-control.html
 -}

import Graphics.UI.GLUT
import Control.Monad.Cont

display :: GLdouble -> IO ()
display y = do
  clear [ColorBuffer]
 
  renderPrimitive LineStrip $ do
    vertex (Vertex2 (-1) (-y))
    vertex (Vertex2 1 y)

  swapBuffers
  postRedisplay Nothing

main = do
  (progname, _) <- getArgsAndInitialize
  initialWindowSize  $= Size 500 500
  initialDisplayMode $= [DoubleBuffered, RGBMode]
  createWindow "Bounce!"

  matrixMode $= Modelview 0
  loadIdentity

  matrixMode $= Projection
  loadIdentity
  ortho (-1) 1 (-1) 1 (-1) 1
  color $ Color4 1 0 0 (1 :: GLfloat)
  imperative
  mainLoop

imperative = flip runContT return $ do
  liftIO $ print "Start!"
  forever $ do
    forM_ [-1, -0.992 .. 1.0] $ \y -> do
      render $ display y
      yield
    liftIO $ print "Bounce!"
    forM_ [-1, -0.992 .. 1.0] $ \y -> do
      render $ display (-y)
      yield
    liftIO $ print "Bounce!"
    yield

render f = liftIO $ displayCallback $= f

--yield = ContT $ \f -> idleCallback $= Just (f () )
yield = ContT $ \f -> addTimerCallback (1000 `div` 30) (f ())
