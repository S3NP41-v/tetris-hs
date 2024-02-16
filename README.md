## {-  What is this?

 its a tetris clone, i really dont think i need to explain what tetris is as it is in the top 3 of best selling games in the world<br>
 and in the rare event that you haven't heard of it, welcome to earth, you must be new here, enjoy your stay!
 
 i expect a LOT of bugs, and so should you!
<br>-}

## {-  Controls

 still controls are up to the interpretation of the author so here they are:

 -arrow keys to move piece around   <br>
 -x        : rotate piece left      <br>
 -c        : rotate piece right     <br>
 -z        : store/unstore piece    <br>
 -space    : slam piece down        <br>
 -q        : exit                   <br>
<br>-}


## {-  Why did i even create this?

 I wanted to code tetris, that is all
 
 well not actually, while a major part, i write code mostly in order to improve my abilities<br>
 this project is no different, thanks to the Functional Programming community on discord for helping me<br>
 and i hope that i will be able to find as well as give help there in the future, because i do plan on leaving the "beginner" state behind me!<br>
<br>-}

## {-  Info About The Tetris Game

 ### Scoring:<br>
  ### levels:<br>
    levels controls how fast a tetromino falls, start at level 0 which is a cell fallen every ??? frames
    level 29: maximum level, tetromino's fall a cell for every frame
    levels advance for every 10 lines cleared (not to confuse with 10 line-clears)
  ### Score:<br>
    40 * (l + 1) for the single line clear
    100 * (l + 1) for the double line clear
    300 * (l + 1) for the triple line clear
    1200 * (l + 1) for the quadruple line clear (a tetris)
    1 for every cell fallen by holding the down key
    where l = current level
 ### Input:<br>
    when a movement key is pressed, the piece will instantly move one grid cell, stop for 16 frames due to delayed auto-shift,<br>
    before moving again once every 6 frames (10 times a second, as the game runs at 60 fps)<br>
<br>-}

## {-  Plans For The Future

 ### A lot of stuff is not implemented, and i am very aware, i wanted to make a somewhat playable game first<br>here is a list of all that i am aware of as of now:

 [ ] Use actual original rotation points for tetrominos, the I and O types rotate around a point i picked,
   in the future i will rotate them around their actual canon points
 
 [ ] better and/or accurate scoring

 [ ] nerf storing and unstoring pieces (honestly i just rushed out this release forgetting i had to do that)

 [ ] Making the UI pretty

 [ ] Save highscore

 [ ] Settings to save/load +defaults (maybe just a .json file?)

 [ ] Music and other SFX

 [ ] A menu (corelated with settings)

 [ ] Frame based input, right now its one press one action,
   so probably move the handling to gameLogicLoop

 [ ] Overall better code, hopefully with the help of FP Discord community this project will be able to serve as an example of learning
   and good code for other beginners.
 
 [ ] Overhaul to the GameState record type and its accompanying functions, i feel like there is a ton of lost potential there
<br>-}