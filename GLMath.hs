{-
 - GLMath.hs
 - By Steven Smith
 -}

module GLMath where

import Graphics.Rendering.OpenGL
import Control.Applicative

class GLVector v where
    cross :: (Num a) => v a -> v a -> v a
    normalizeV :: (Num a, Floating a) => v a -> v a
    dot :: (Num a) => v a -> v a -> a
    lengthV :: (Num a, Floating a) => v a -> a

class GLVector3 v where
    toVector3 :: v a -> Vector3 a
    toVertex3 :: v a -> Vertex3 a
    toNormal3 :: v a -> Normal3 a

-- Vector

instance (Num a) => Num (Vector3 a) where
    (+) = liftA2 (+)
    (*) = liftA2 (*)
    (-) = liftA2 (-)
    negate = liftA negate
    abs = liftA abs
    signum = liftA signum
    fromInteger = pure . fromInteger

instance (Fractional a) => Fractional (Vector3 a) where
    (/) = liftA2 (/)
    recip = liftA recip
    fromRational = pure . fromRational

instance GLVector Vector3 where
    cross (Vector3 x1 y1 z1) (Vector3 x2 y2 z2) =
        Vector3 ((y1 * z2) - (z1 * y2))
                ((z1 * x2) - (x1 * z2))
                ((x1 * y2) - (y1 * x2))
    normalizeV v = v / (pure $ lengthV v)
    dot (Vector3 x1 y1 z1) (Vector3 x2 y2 z2) = (x1 * x2) + (y1 * y2) + (z1 * z2)
    lengthV (Vector3 x y z) = sqrt $ (x * x) + (y * y) + (z * z)

instance GLVector3 Vector3 where
    toVector3 = id
    toVertex3 (Vector3 x y z) = Vertex3 x y z
    toNormal3 (Vector3 x y z) = Normal3 x y z

-- Vertex

instance (Num a) => Num (Vertex3 a) where
    (+) = liftA2 (+)
    (*) = liftA2 (*)
    (-) = liftA2 (-)
    negate = liftA negate
    abs = liftA abs
    signum = liftA signum
    fromInteger = pure . fromInteger

instance (Fractional a) => Fractional (Vertex3 a) where
    (/) = liftA2 (/)
    recip = liftA recip
    fromRational = pure . fromRational

instance GLVector Vertex3 where
    cross (Vertex3 x1 y1 z1) (Vertex3 x2 y2 z2) =
        Vertex3 ((y1 * z2) - (z1 * y2))
                ((z1 * x2) - (x1 * z2))
                ((x1 * y2) - (y1 * x2))
    normalizeV v = v / (pure $ lengthV v)
    dot (Vertex3 x1 y1 z1) (Vertex3 x2 y2 z2) = (x1 * x2) + (y1 * y2) + (z1 * z2)
    lengthV (Vertex3 x y z) = sqrt $ (x * x) + (y * y) + (z * z)

instance GLVector3 Vertex3 where
    toVector3 (Vertex3 x y z) = Vector3 x y z
    toVertex3 = id
    toNormal3 (Vertex3 x y z) = Normal3 x y z

-- Normal

instance (Num a) => Num (Normal3 a) where
    (+) = liftA2 (+)
    (*) = liftA2 (*)
    (-) = liftA2 (-)
    negate = liftA negate
    abs = liftA abs
    signum = liftA signum
    fromInteger = pure . fromInteger

instance (Fractional a) => Fractional (Normal3 a) where
    (/) = liftA2 (/)
    recip = liftA recip
    fromRational = pure . fromRational

instance GLVector Normal3 where
    cross (Normal3 x1 y1 z1) (Normal3 x2 y2 z2) =
        Normal3 ((y1 * z2) - (z1 * y2))
                ((z1 * x2) - (x1 * z2))
                ((x1 * y2) - (y1 * x2))
    normalizeV v = v / (pure $ lengthV v)
    dot (Normal3 x1 y1 z1) (Normal3 x2 y2 z2) = (x1 * x2) + (y1 * y2) + (z1 * z2)
    lengthV (Normal3 x y z) = sqrt $ (x * x) + (y * y) + (z * z)

instance GLVector3 Normal3 where
    toVector3 (Normal3 x y z) = Vector3 x y z
    toVertex3 (Normal3 x y z) = Vertex3 x y z
    toNormal3 = id
