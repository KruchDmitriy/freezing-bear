import Graphics.UI.GLUT
import Graphics.Rendering.OpenGL
import System.Exit ( exitWith, ExitCode(ExitSuccess) )

import ExpressionParser.ExpressionParser

data DescriptorFunc = DescriptorFunc {
    f     :: Function,
    a     :: Double,
    b     :: Double,
    step  :: Double,
    color :: Color4 GLfloat
}

type QueueOfFunc = [DescriptorFunc]

add_func :: QueueOfFunc -> DescriptorFunc -> QueueOfFunc
add_func queue d_func = d_func : queue

find_left :: QueueOfFunc -> GLfloat
find_left queue = realToFrac . minimum $ map a queue

find_right :: QueueOfFunc -> GLfloat
find_right queue = realToFrac . maximum $ map b queue

get_vertexes :: DescriptorFunc -> [(GLfloat, GLfloat)]
get_vertexes (DescriptorFunc f a b step _) =
    map (\(x, y) -> (realToFrac x, realToFrac y)) $
    filter (\(x, y) -> abs y /= inf && not (isNaN y))
        $ zip xs (map (evaluate_func f) xs)
        where
            xs = [a, a + step ..  b]
            inf = read "Infinity" :: Double

f_min :: DescriptorFunc -> GLfloat
f_min f = minimum $ map snd $ get_vertexes f

f_max :: DescriptorFunc -> GLfloat
f_max f = maximum $ map snd $ get_vertexes f

find_top :: QueueOfFunc -> GLfloat
find_top queue = maximum maxs
    where maxs = map f_max queue

find_bottom :: QueueOfFunc -> GLfloat
find_bottom queue = minimum mins
    where mins = map f_min queue

draw_window :: QueueOfFunc -> IO ()
draw_window queue_func = do
    (progName, args) <- getArgsAndInitialize
    initialWindowSize $= Size 500 500
    initialWindowPosition $= Position 100 100
    createWindow "Plot"
    displayCallback $= display queue_func
    keyboardMouseCallback $= Just keyboard
    mainLoop

keyboard :: KeyboardMouseCallback
keyboard (Char '\27') Down _ _ = exitWith ExitSuccess
keyboard _            _    _ _ = return ()

ortho2D' :: GLfloat -> GLfloat -> GLfloat -> GLfloat -> IO ()
ortho2D' a b c d =
    ortho2D (realToFrac a) (realToFrac b) (realToFrac c) (realToFrac d)

display :: QueueOfFunc -> IO ()
display queue_func = do
    clearColor $= Color4 1 1 1 1
    clear [ColorBuffer]
    windowPixelSize <- get windowSize
    loadIdentity
    ortho2D'  a_s b_s c_s d_s
    draw_grid a_s b_s c_s d_s
    draw_axes a_s b_s c_s d_s windowPixelSize
    mapM_ (\ x -> draw_prim LineStrip (get_vertexes x) (Main.color x)) queue_func
    flush
    where
            a_s = a - sx
            b_s = b + sx
            c_s = c - sy
            d_s = d + sy
            sx  = realToFrac $ (b - a) / 30
            sy  = realToFrac $ (d - c) / 30

            a = find_left   queue_func
            b = find_right  queue_func
            c = find_bottom queue_func
            d = find_top    queue_func

draw_prim :: PrimitiveMode -> [(GLfloat, GLfloat)] -> Color4 GLfloat -> IO ()
draw_prim pmode vertexes color = do
    renderPrimitive pmode $ do
        currentColor $= color
        mapM_ (\ (x,y) -> vertex $ Vertex2 x y) vertexes

draw_grid :: GLfloat -> GLfloat -> GLfloat -> GLfloat -> IO ()
draw_grid a b c d = draw_prim Lines (create_grid a b c d)
                                    (Color4 0.75 0.75 0.75 1)

create_grid :: GLfloat -> GLfloat -> GLfloat -> GLfloat -> [(GLfloat, GLfloat)]
create_grid a b c d =
    [(x, y) | x <- [a, a + step_x .. b], y <- [c, d]] ++
    map flip' [(x, y) | x <- [c, c + step_y .. d], y <- [a, b]]
    where
        flip' = \ (x, y) -> (y, x)
        step_x = (b - a) / 10
        step_y = (d - c) / 10

draw_axes :: GLfloat -> GLfloat -> GLfloat -> GLfloat -> Size -> IO ()
draw_axes a b c d (Size w h) = do
    renderPrimitive Lines $ do
        currentColor $= Color4 0 0 0 1
        vertex $ Vertex2 a 0.0
        vertex $ Vertex2 b 0.0
        vertex $ Vertex2 (0.0  :: GLfloat) c
        vertex $ Vertex2 (0.0  :: GLfloat) d
    renderPrimitive Triangles $ do
        currentColor $= Color4 0 0 0 1
        vertex $ Vertex2 (b      :: GLfloat) 0.0
        vertex $ Vertex2 (b - dx :: GLfloat) dy
        vertex $ Vertex2 (b - dx :: GLfloat) (-dy)
        vertex $ Vertex2 (0.0    :: GLfloat) d
        vertex $ Vertex2 dx    (d - dy)
        vertex $ Vertex2 (-dx) (d - dy)
        where
            dx = (b - a) / (fromIntegral w) * 5
            dy = (d - c) / (fromIntegral h) * 5

drawl :: IO ()
drawl = do
    let descr1 = DescriptorFunc (create_func "x^(0.5)") (-3) 10 0.01 (Color4 0 0 1 1)
        descr2 = DescriptorFunc (create_func "sin(x)") 0 10 0.01 (Color4 0 1 0 1)
        descr3 = DescriptorFunc (create_func "x - sin(x)") 0 10 0.01 (Color4 1 0 0 1)
        descr4 = DescriptorFunc (create_func "((3/10)*x^3) + ((13/6)*x^2) - ((62/15)*x) + 1") 0 5 0.01 (Color4 1 0 1 1)
        queue = add_func (add_func (add_func (add_func [] descr1) descr2) descr3) descr4 in draw_window queue

