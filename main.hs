module Main(main) where


-- import nedded libraries 
import Graphics.Gloss
import Graphics.Gloss.Data.ViewPort
import Graphics.Gloss.Interface.Pure.Game

-- window size
width, height, offset :: Int
width = 850
height = 500
offset = 100

window :: Display
window = InWindow "Breakout" (width, height) (offset, offset)

background :: Color
background = black


-- number of frames to show per second.
fps :: Int
fps = 60

-- radius of ball
radius :: Float
radius = 10

-- walls
width1,width2,height1,height2 :: Float
-- top & bottom walls
width1 = 550
height1 = 10
-- left & right walls
width2 = 10
height2 = 400 

-- paddle
paddleWidth,paddleHeight :: Float
paddleWidth = 150
paddleHeight = 26

-- block
blockWidth,blockHeight :: Float
blockWidth = 86
blockHeight = 30

-- data describing the state of the game
data BreakoutGame = Game
    { ballLoc :: (Float, Float)  -- ball (x, y) location
    , ballVel :: (Float, Float)  -- ball (x, y) velocity
    , playerX :: Float           -- paddle x position 
    , playerY :: Float           -- paddle y position
    , block :: [(Float,Float)]   -- list of blocks
    , gameScore :: Int           -- score             
    , gameState :: String        -- win or lost text
    , gameLives :: Int           -- lives
    } deriving Show 

-- the starting state for the game of Pong
initialState :: BreakoutGame
initialState = Game
    { ballLoc = (0, -75)
    , ballVel = (60, 60)
    , playerX = 0
    , playerY = -150
    , block = [(-200, 100),(-100 , 100),(1 , 100),(100 , 100),(200 , 100),
    (-200 , 60),(-100 , 60),(1 , 60),(100 , 60),(200 , 60),
    (-200 ,20),(-100 , 20),(1 , 20),(100 , 20),(200 , 20),
    (-200 , -20),(-100 , -20) , (1 , -20) , (100 , -20) , (200 , -20)]
    , gameScore = 0 
    , gameState = ""  
    , gameLives = 3      
    }




-- convert a game state into a picture
render :: BreakoutGame -> Picture  
render game =
    pictures [ball, walls,
                mkPaddle  violet (playerX game) (playerY game),
                blocks violet ((block game) !! 0) , blocks violet ((block game) !! 1) , blocks violet ((block game) !! 2), blocks violet ((block game) !! 3), blocks violet ((block game) !! 4),
                blocks violet ((block game) !! 5) ,blocks violet ((block game) !! 6),blocks violet ((block game) !! 7),blocks violet ((block game) !! 8),blocks violet ((block game) !! 9),
                blocks violet ((block game) !! 10) ,blocks violet ((block game) !! 11),blocks violet ((block game) !! 12),blocks violet ((block game) !! 13),blocks violet ((block game) !! 14),
                blocks violet ((block game) !! 15), blocks violet ((block game) !! 16) , blocks violet ((block game) !! 17) , blocks violet ((block game) !! 18) , blocks violet ((block game) !! 19),
                score orange (gameScore game) , state violet (gameState game) , lives orange (gameLives game)
            ]     
    where
        -- ball
        ball = uncurry translate (ballLoc game) $ color ballColor $ circleSolid radius
        ballColor = orange

        -- walls
        wall :: Float -> Float -> Float -> Float -> Picture
        wall width height x y =
            translate x y $
                color wallColor $
                rectangleSolid width height

        wallColor = violet
        walls = pictures [wall width1 height1 0 200, wall width1 height1 0 (-200) , wall width2 height2 (-270) 0 , wall width2 height2 (270) 0]

        --  paddle
        mkPaddle :: Color -> Float -> Float -> Picture
        mkPaddle col x y = pictures[translate x y $ color col $ rectangleSolid paddleWidth paddleHeight]
        paddleColor = dark $ dark violet

        --  blocks
        blocks :: Color -> (Float , Float) -> Picture
        blocks col (x , y) = pictures[translate x y $ color col $ rectangleSolid blockWidth blockHeight , translate x y $ color blocksColor $ rectangleSolid (blockWidth-6) (blockHeight-10)]
        blocksColor = dark $ dark violet    
        
        -- score
        score :: Color -> Int -> Picture 
        score col txt = translate (-420) (170) $ scale 0.18 0.18 $ color col $ Text ("Score : " ++ show txt)

        -- state
        state :: Color -> String -> Picture 
        state col txt = if (gameScore game) == 200
                        then translate (-90) (150) $ scale 0.3 0.3 $ color orange $ Text (show "YOU WIN!!")
                        else if (gameLives game) == 0
                            then translate (-90) (150) $ scale 0.3 0.3 $ color orange $ Text (show "YOU LOSE!!")
                        else translate (0) (160) $ scale 0.3 0.3 $ color black $ Text (show txt)

        -- lives
        lives :: Color -> Int -> Picture 
        lives col txt = translate (300) (170) $ scale 0.18 0.18 $ color col $ Text ("Lives : " ++ show txt)                

-- Update the ball position using its current velocity.
-- moveBall :: number of seconds since last update -> initial game state -> new game state with an updated ball position 
moveBall :: Float -> BreakoutGame -> BreakoutGame      
moveBall seconds game = game { ballLoc = (x', y') }
    where
        -- old locations and velocities
        (x, y) = ballLoc game
        (vx, vy) = ballVel game

        -- new locations
        -- if condition to stop ball movement in case of win or lose
        x' = if (gameLives game == 0) || (gameScore game == 200)
            then 0
            else x + vx * seconds
        y' =  if (gameLives game == 0) || (gameScore game == 200)
            then 0
            else y + vy * seconds 


-- collisions 
-- WALL COLLISION
-- given position and radius of the ball, return whether a collision occurred with walls
-- top & bottom walls
wallCollision1 ::  (Float, Float) -> Float -> Bool
wallCollision1 (_, ballY) radius = topCollision
    where
        topCollision = ballY + radius >=  200

bottomCollision ::  (Float, Float) -> Float -> Bool
bottomCollision (_, ballY) radius = bottomCollision
    where
        bottomCollision = ballY - radius <= -200

-- left & right walls
wallCollision2 :: (Float, Float) -> Float -> Bool 
wallCollision2 (ballX, _) radius = rightCollision || leftCollision
    where
        leftCollision = ballX - radius <= -270
        rightCollision = ballX + radius >=  270  

-- ball bounces when hitting balls 
wallBounce :: BreakoutGame -> BreakoutGame
wallBounce game = game { ballVel = (vx', vy') , gameLives = gameLives' }
    where
        -- old velocities
        (vx, vy) = ballVel game

        -- top & bottom walls collision
        vy' = if wallCollision1 (ballLoc game) radius || bottomCollision (ballLoc game) radius
            then
                -- update the velocity
                -vy
            else
                -- return the old velocity.
                vy 
        -- left & right walls collision        
        vx' = if wallCollision2 (ballLoc game) radius 
            then
                -- update the velocity
                -vx
            else
                -- return the old velocity.
                vx         
        gameLives'= if bottomCollision (ballLoc game) radius 
                    then (gameLives game) - 1
                    else (gameLives game)
        



-- PADDLE COLLISION
-- detect collision with paddle
paddleCollision :: BreakoutGame -> Bool
paddleCollision game = collision
    where
        -- location of ball
        (ballX, ballY) = ballLoc game
        -- check if ball within area of paddle     
        collision = (ballX - radius < (playerX game) + paddleWidth/2) && (ballX + radius > (playerX game) -  paddleWidth/2)&& (ballY + radius <= (playerY  game) + paddleHeight)

-- ball bonuce when hitting paddle
paddleBounce :: BreakoutGame -> BreakoutGame
paddleBounce game = game { ballVel = (vx, vy')}
                            
    where 
        -- old velocity of ball
        (vx,vy) = ballVel game
        -- location of ball
        (ballX, ballY) = ballLoc game
        -- check collision 
        collision = paddleCollision game
        vy' = if collision
            then 
                -- update velocity
                -vy
            else 
                -- return old velocity
                vy   


-- BLOCKS COLLISION
-- takes x,y of block as tupple from list of blocks and checks for collision with ball
blockCollision :: BreakoutGame -> (Float,Float) -> Bool
blockCollision game (x,y)  = if collision
                            then True
                            else False
    where
        -- location of the ball 
        (ballX, ballY) = ballLoc game
        -- check whether ball is within block area
        collision = (ballX - radius < x + blockWidth/2) && (ballX + radius > x -  blockWidth/2 )&& (ballY + radius == y)
        

-- ball bounces if it hits the block , then block is removed and score is incremented         
blockBounce :: BreakoutGame -> BreakoutGame
blockBounce game = game { ballVel = (vx, vy') , block = [(x1,y1),(x2,y2),(x3,y3),(x4,y4),(x5,y5),
                    (x6,y6),(x7,y7),(x8,y8),(x9,y9),(x10,y10),
                (x11,y11),(x12,y12),(x13,y13),(x14,y14),(x15,y15),
                (x16,y16),(x17,y17),(x18,y18),(x19,y19),(x20,y20)] ,
                gameScore = score}
    where 
        -- old velocity of ball
        (vx,vy) = ballVel game
        -- check collision with each block
        collision1 = blockCollision game ((block game) !! 0) 
        collision2 = blockCollision game ((block game) !! 1) 
        collision3 = blockCollision game ((block game) !! 2) 
        collision4 = blockCollision game ((block game) !! 3) 
        collision5 = blockCollision game ((block game) !! 4) 

        collision6 = blockCollision game ((block game) !! 5) 
        collision7 = blockCollision game ((block game) !! 6) 
        collision8 = blockCollision game ((block game) !! 7) 
        collision9 = blockCollision game ((block game) !! 8) 
        collision10 = blockCollision game ((block game) !! 9) 

        collision11 = blockCollision game ((block game) !! 10) 
        collision12 = blockCollision game ((block game) !! 11) 
        collision13 = blockCollision game ((block game) !! 12) 
        collision14 = blockCollision game ((block game) !! 13) 
        collision15 = blockCollision game ((block game) !! 14) 

        collision16 = blockCollision game ((block game) !! 15) 
        collision17 = blockCollision game ((block game) !! 16) 
        collision18 = blockCollision game ((block game) !! 17) 
        collision19 = blockCollision game ((block game) !! 18) 
        collision20 = blockCollision game ((block game) !! 19) 

        -- check whether to increment score or not
        score = if collision1 || collision2 || collision3 || collision4 || collision5 || collision6 || collision7 || collision8 || collision9 || collision10 || collision11 || collision12 || collision13 || collision14 || collision15 || collision16 || collision17 || collision18 || collision19 || collision20
                -- increment if collision 
                then addScore (gameScore game)
                -- keep old score
                else (gameScore game)

                    
        -- velocitty 
        vy' = if collision1 || collision2 || collision3 || collision4 || collision5 || collision6 || collision7 || collision8 || collision9 || collision10 || collision11 || collision12 || collision13 || collision14 || collision15 || collision16 || collision17 || collision18 || collision19 || collision20
            -- update velocity if collision occured 
            then 
                if (vy<0)
                then vy    
                else -vy
            -- keep old velocity
            else 
                vy

        -- move block from its place to postion (-350,100) if collision occured
        x1 = if collision1
            then -350
            else fst ((block game) !! 0)    
        y1 = if collision1
            then 100
            else snd ((block game) !! 0) 

        x2 = if collision2
            then -350
            else fst ((block game) !! 1)    
        y2 = if collision2
            then 100
            else snd ((block game) !! 1) 

        x3 = if collision3
            then -350
            else fst ((block game) !! 2)   
        y3 = if collision3
            then 100
            else snd ((block game) !! 2) 

        x4 = if collision4
            then -350
            else fst ((block game) !! 3)   
        y4 = if collision4
            then 100
            else snd ((block game) !! 3) 

        x5 = if collision5
            then -350
            else fst ((block game) !! 4)   
        y5 = if collision5
            then 100
            else snd ((block game) !! 4)  

        x6 = if collision6
            then -350
            else fst ((block game) !! 5)   
        y6 = if collision6
            then 100
            else snd ((block game) !! 5)

        x7 = if collision7
            then -350
            else fst ((block game) !! 6)   
        y7 = if collision7
            then 100
            else snd ((block game) !! 6)    

        x8 = if collision8
            then -350
            else fst ((block game) !! 7)   
        y8 = if collision8
            then 100
            else snd ((block game) !! 7)

        x9 = if collision9
            then -350
            else fst ((block game) !! 8)   
        y9 = if collision9
            then 100
            else snd ((block game) !! 8)  

        x10 = if collision10
            then -350
            else fst ((block game) !! 9)   
        y10 = if collision10
            then 100
            else snd ((block game) !! 9)  

        x11 = if collision11
            then -350
            else fst ((block game) !! 10)   
        y11 = if collision11
            then 100
            else snd ((block game) !! 10)  

        x12 = if collision12
            then -350
            else fst ((block game) !! 11)   
        y12 = if collision12
            then 100
            else snd ((block game) !! 11) 

        x13 = if collision13
            then -350
            else fst ((block game) !! 12)   
        y13 = if collision13
            then 100
            else snd ((block game) !! 12) 

        x14 = if collision14
            then -350
            else fst ((block game) !! 13)   
        y14 = if collision14
            then 100
            else snd ((block game) !! 13) 

        x15 = if collision15
            then -350
            else fst ((block game) !! 14)   
        y15 = if collision15
            then 100
            else snd ((block game) !! 14)  

        x16 = if collision16
            then -350
            else fst ((block game) !! 15)   
        y16 = if collision16
            then 100
            else snd ((block game) !! 15)

        x17 = if collision17
            then -350
            else fst ((block game) !! 16)   
        y17 = if collision17
            then 100
            else snd ((block game) !! 16)  

        x18 = if collision18
            then -350
            else fst ((block game) !! 17)   
        y18 = if collision18
            then 100
            else snd ((block game) !! 17)

        x19 = if collision19
            then -350
            else fst ((block game) !! 18)   
        y19 = if collision19
            then 100
            else snd ((block game) !! 18)    

        x20 = if collision20
            then -350
            else fst ((block game) !! 19)   
        y20 = if collision20
            then 100
            else snd ((block game) !! 19)     

-- increase socre by 10 when hitting a brick
addScore :: Int -> Int 
addScore = (+ 10)

-- update the game by moving the ball and bouncing off walls m paddle , blocks
update :: Float -> BreakoutGame -> BreakoutGame  
update seconds = wallBounce . paddleBounce . blockBounce . moveBall seconds 



-- respond to key events.
handleKeys :: Event -> BreakoutGame -> BreakoutGame

-- For an 's' keypress, reset the ball to the center
handleKeys (EventKey (Char 's') _ _ _) game = game { ballLoc = (0, 0) }
-- For an 'a' keypress, move paddle left
handleKeys (EventKey (Char 'a') _ _ _) game = game { playerX = x }
    where 
        -- checks first if paddle is in walls boundries
        x = if playerX game > -200
            then playerX game - 10
            else playerX game
-- For an 'd' keypress, move paddle right
handleKeys (EventKey (Char 'd') _ _ _) game = game { playerX = x }
    where 
        -- checks first if paddle is in walls boundries 
        x = if playerX game < 200
            then playerX game + 10
            else playerX game
-- do nothing for all other events
handleKeys _ game = game

-- |main function
main :: IO ()
main = play window background fps initialState render handleKeys update