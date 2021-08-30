TITLE MASM Template	(main.asm)
; Description: Snake for Assembler (Console Version)
; Goal:        A working reproduction of the Snake game with varying difficulty and basic graphics.

INCLUDE Irvine32.inc ;Required as seprate download, used for colors and some other things.    
INCLUDE MACROS.inc

.data

;Variables/Arrays-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
consoleHandle DWORD ? ;Used for several processes to work.

titleStr BYTE "-----Assembler Snake-----",0 ;Title of the console for the program

maxX              = 39                  ;Grid X coord limit (Top left is (0,0))
maxY              = 19                  ;Grid Y coord limit
numFood           = 20                  ;number of food items to be on field 
numSegments       = (maxX+1) * (maxY+1)+100 ;number of possible snake segments
numSegmentsInPlay DWORD 2               ;number of segments on the grid (Always leave more than 1!!).

;------The colors defined in Irvine32.inc are:
;black, white, brown, yellow, blue, green, cyan, red, magenta, gray, lightBlue, lightGreen, lightCyan, lightRed, lightMagenta, and lightGray.    
 
foodColor         = (yellow * 16)       ;color to use for food.
snakeColor        = (red * 16)          ;color to use for snake.
backgroundColor   = (green * 16)        ;color to use for background.

delayTime DWORD 200                     ;number of milliseconds to delay before refreshing the animation.
snakeMoveDirection COORD <0,1>          ;Set direction of snake movement. Default to down at start. NOTE* the dimensions for y are backwards! ie 1 is down and -1 is up!!
windowRect SMALL_RECT <0,0,maxX,maxY>   ;Set variables for the grid size.
cursorInfo CONSOLE_CURSOR_INFO <100,0>  ;Values to set cursor to invisible.

foodArray  COORD numFood DUP(<?,?>)     ;Define the food array for correct placement of the food pieces on the grid (numFood manipulates how much food is out.)
snakeArray COORD numSegments DUP(<?,?>) ;Define the snake array for correct placement of the snake segments on the grid (numSegments manipulates how long the snake is) NOTE* Vectors are much more appreciated now than ever before..
;Structs---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

COORD STRUCT   ;Coordinate system for board manipulation, cursor placement, movement, etc.
	X WORD ?   ;Horizontal
	Y WORD ?   ;Vertical
COORD ENDS

SMALL_RECT STRUCT  ;Used to set up the grid size and boundries for the game.
	Left   WORD ?
	Top    WORD ?
	Right  WORD ?
	Bottom WORD ?
SMALL_RECT ENDS

CONSOLE_CURSOR_INFO STRUCT
	dwSize   DWORD ?
	bVisible DWORD ?
CONSOLE_CURSOR_INFO ENDS

;Prototypes-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
.code

PlayGame       PROTO
SetFood        PROTO, numberOfFood:DWORD
RegenerateFood PROTO

;Start Code-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

main proc

;<Pre Game Stuff>
	call Randomize   ; Set the random seed.
	
	
	INVOKE SetConsoleTitle, ADDR titleStr  ;Sets the console title
	INVOKE GetStdHandle, STD_OUTPUT_HANDLE ;Get ready to handle console interactions.
	mov consoleHandle, eax

	INVOKE SetConsoleWindowInfo, consoleHandle, TRUE, ADDR windowRect ;Set the grid size for the game.
	INVOKE SetConsoleCursorInfo, consoleHandle, ADDR cursorInfo       ;Set cursor to invisible.
	
	;Set Board--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
	mov eax,(backgroundColor)
	call SetTextColor
	mov ecx, (maxY+1)
	L1: ;Set up the grid background. USES ECX
		dec cl                ;Because the grid starts at (0,0) we need one less than cl on the coords
		mGotoxy     0, cl     ;Macro to set the cursor at the correct row (cl is the lower end of ecx and is needed for the correct size in the macro).
		mWriteSpace (maxX+1)  ;Macro to fill the row with color.
		inc cl                ;Restore ecx for the loop counter
	loop L1

	;Set Snake Head---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
	mov esi, OFFSET snakeArray
	mov (COORD PTR [esi]).X, ((maxX-1)/2) ;Set beginning head of snake x coordinate
	mov (COORD PTR [esi]).Y, ((maxY-1)/2) ;Set beginning head of snake y coordinate

	mov eax,(snakeColor)
	call SetTextColor
	mov ax, (COORD PTR [esi]).X
	mov bx, (COORD PTR [esi]).Y
	mgotoxy al, bl
	mWriteSpace 1

	;Set Food---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
	;Could implement a condition to not put food on top of itself in the future, for now there is just extra nutritious food on the map rarely.
	
	INVOKE SetFood, numFood

;</Pre Game Stuff>

	mov ecx, 0 ;Set 0 for infitnite loop
	GameLoop:
		INVOKE PlayGame
	loop GameLoop
	
	invoke ExitProcess,0   
main endp

;-------------------------------------------
; PlayGame
; Starts the loop that functions as the game animation and deals with UI to play the game.
; Pointer to snakearray, foodArray, and snakeMoveDirection.
; Returns nothing.
;-------------------------------------------
PlayGame PROC USES eax ebx ecx esi edi,
	
	;Set Snake Movement Direction--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
	mov esi, OFFSET snakeArray          ;Set up esi with snake array
	mov edi, OFFSET snakeMoveDirection  ;Set up edi with movement direction
	mov ah, 0    ;Refresh ah for codes
	call ReadKey ;Call no wait function to read the key and store scan codes in ah, compare with the scan codes for the arrow keys.
	
	cmp ah, 4Bh						;Left
	je Left
	jmp NotLeft
	Left:
		cmp (COORD PTR [edi]).X, 1  ;if moving the opposite direction, don't allow the snake to run over itself.
		je NotLeft
		mov (COORD PTR [edi]).X, -1 ;else Snake is set to move left now.
		mov (COORD PTR [edi]).Y, 0  ;set Vertical movement to 0.
	jmp EndInput
	NotLeft:

	cmp ah, 4Dh						;Right
	je Right
	jmp NotRight
	Right:
		cmp (COORD PTR [edi]).X, -1 ;if moving the opposite direction, don't allow the snake to run over itself.
		je NotRight
		mov (COORD PTR [edi]).X, 1  ;else Snake is set to move right now.
		mov (COORD PTR [edi]).Y, 0  ;set vertical movement to 0.
	jmp EndInput
	NotRight:

	cmp ah, 48h						;Up
	je Up
	jmp NotUp
	Up:
		cmp (COORD PTR [edi]).Y, 1  ;if moving the opposite direction, don't allow the snake to run over itself.
		je NotUp
		mov (COORD PTR [edi]).Y, -1 ;else Snake is set to move up now.
		mov (COORD PTR [edi]).X, 0  ;set horizontal movement to 0.
	jmp EndInput
	NotUp:

	cmp ah, 50h						;Down
	je Down
	jmp EndInput                    ;No Arrow Keys were pressed, move out of input phase.
	Down:
		cmp (COORD PTR [edi]).Y, -1 ;if moving the opposite direction, don't allow the snake to run over itself.
		je EndInput
		mov (COORD PTR [edi]).Y, 1  ;else Snake is set to move down now.
		mov (COORD PTR [edi]).X, 0  ;set horizontal movement to 0.

	EndInput: ;End of the arrow checking portion.

	;Update snake graphic and check conditions for loss/eat food---------------------------------------------------------------------------------------------------------------------------------------------------

	;update last element space with background color after snake moves off of it.
	mov eax,(backgroundColor)                   ;Set color back to background color.
	call SetTextColor

	mov edx, numSegmentsInPlay
	mov ax, (COORD PTR [esi+(edx*4-4)]).X
	mov bx, (COORD PTR [esi+(edx*4-4)]).Y
	mgotoxy al,bl
	mWriteSpace 1

	mov eax,(snakeColor)  ;Set color back to snake color.
	call SetTextColor

	dec edx
	mov ecx, edx ;Loop as many times as necissary to move all body segments of the snake.
	LSnakeBodyAnimation:
		mov esi, OFFSET snakeArray
		
		mov ax, (COORD PTR [esi+(ecx*4-4)]).X  ;update data for this element, move it to the previous element's position. (Remember COORD is 4 wide)
		mov bx, (COORD PTR [esi+(ecx*4-4)]).Y
		mov (COORD PTR [esi+ecx*4]).X, ax
		mov (COORD PTR [esi+ecx*4]).Y, bx
		mgotoxy al,bl                      ;move to this segment
		mWriteSpace 1                      ;color the segment

	loop LSnakeBodyAnimation

	;update head position
	mov esi, OFFSET snakeArray  ;reset esi to point at beginning
	mov ax, (COORD PTR [edi]).X ;set up to use add for X
	mov bx, (COORD PTR [edi]).Y ;set up to use add for Y
	add (COORD PTR [esi]).X, ax ;Update to new head position
	add (COORD PTR [esi]).Y, bx 
	mov ax, (COORD PTR [esi]).X ;update ax and bx to print using macro
	mov bx, (COORD PTR [esi]).Y
	mgotoxy al, bl
	mWriteSpace 1

	cmp (COORD PTR [esi]).X, (maxX + 1) ; Off right side of grid.
	je  GameOver						; Gameover else check next condition.
	cmp (COORD PTR [esi]).X, -1         ; Off left side of grid.
	je  GameOver                       
	cmp (COORD PTR [esi]).Y, (maxY + 1) ; Off bottom side of grid.
	je  GameOver
	cmp (COORD PTR [esi]).Y, -1         ; Off Upper side of grid.
	je  GameOver

	
	mov ecx, numSegmentsInPlay ;Check against all segments - head. NOTE* probably should be less as the snake can't hit the second element etc.
	HitSelfLoop:
		mov esi, OFFSET snakeArray             ;Reset ptr to beginning of snake array.

		mov ax, (COORD PTR [esi+(ecx*4)]).X    ;Set each element to be checked starting with the tail end.
		cmp (COORD PTR [esi]).X, ax            ;check each element against head to see if a collision happened on X, if so check Y.
		jne  OK1							   ;Skip to next element if no match of X     

		mov ax, (COORD PTR [esi+(ecx*4)]).Y   ;Set each element to be checked starting with the tail end.
		cmp (COORD PTR [esi]).Y, ax           ;check each element against head to see if a collision happened, if so GameOver.
		je  GameOver


		OK1:
	loop HitSelfLoop
	

	mov ecx, numFood
	eatLoop: ;Test to see if the snake ate something
		mov esi, OFFSET snakeArray
		mov edi, OFFSET foodArray
		
		mov ax, (COORD PTR [edi+(ecx*4)-4]).X ;if snake x coord = food x coord, test y else jump to next element.
		cmp (COORD PTR [esi]).X, ax
		jne OK2
	
		mov bx, (COORD PTR [edi+(ecx*4)-4]).Y ;if snake y coord = food y coord, the snake ate something else jump to next element.
		cmp (COORD PTR [esi]).Y, bx
		jne OK2

		inc numSegmentsInPlay  ;Increase the snake length.
		
		INVOKE RegenerateFood  ;Get some new random numbers for the food array element.

		mov (COORD PTR [edi+(ecx*4)-4]).X, dx ;Replace old array element X and Y NOTE* I wish these were in the process, but everything I tried acted strange. Something to look into later.
		mov (COORD PTR [edi+(ecx*4)-4]).Y, ax

		;Print new food to grid.
		mgotoxy dl, al
		mov eax,(foodColor)
		call SetTextColor
		mWriteSpace 1
		OK2:
	loop eatLoop

	;Delay for pseudo animation/difficulty.------------------------------------------------------------------------------------------------------------------------------------------------------------------------
	INVOKE Sleep, delayTime ;Invoke the delay before the snake moves again	

	ret

	GameOver:
	mgotoxy ((maxX-1)/2), ((maxY-1)/2)                                ;Center cursor for loss message.
	mWrite <"Game Over.", 0dh, 0ah>                                   ;Print loss message.
	INVOKE SetConsoleWindowInfo, consoleHandle, TRUE, ADDR windowRect ;Recenter the window.
	INVOKE Sleep, 2000                                              ;Pause so the user can realize they lost before the exit on any key is displayed.
	invoke ExitProcess,0                                              ;End Game
PlayGame ENDP

SetFood PROC USES esi edi eax ebx,
	numberOfFood:DWORD,

	mov eax,(foodColor) ;Food color
	call SetTextColor


	mov ecx, numberOfFood
	L2: ;Set the food in random positions on the board.
		mov esi, OFFSET foodArray
		mov eax, maxX    ; Set random range number to max X (0 through maxX).
		call RandomRange ; Get random number and store in eax

		;Check if X is occupied
		mov edi, OFFSET snakeArray    ;set edi to point to the snake array
		cmp ax, (COORD PTR [edi]).X   ;compare for equality with the offered random number
		je L2                         ;if snakeHead.X = eax, restart process for this element.

		mov (COORD PTR [esi+(ecx*4)-4]).X,ax ; else give the OK to put the number in X for this element.

		mov esi, OFFSET foodArray
		mov eax, maxY    ; Set random range number to max Y (0 through maxY).
		call RandomRange ; Get random number and store in eax

		;Check if Y is occupied
		mov edi, OFFSET snakeArray    ;set edi to point to the snake array
		cmp ax, (COORD PTR [edi]).Y   ;compare for equality
		je L2                         ;if snakeHead.Y = eax, restart process for this element.
			
		mov (COORD PTR [esi+(ecx*4)-4]).Y,ax ; else give the OK to put the number in Y for this element.
		
		;Put in the food on the grid after the numbers have been checked and passed.
		mov ax, (COORD PTR [esi+(ecx*4)-4]).X
		mov bx, (COORD PTR [esi+(ecx*4)-4]).Y

		mgotoxy al,bl
		mWriteSpace 1

	loop L2
	
	ret
SetFood ENDP

RegenerateFood PROC USES esi edi ebx ecx, ;Regenerates food when it has been eaten. Cannot place on snake or other food.
	
	RNGHarder:       ;If numbers fail a test, generate new ones and try again.

	mov eax, maxX    ; Set random range number to max X (0 through maxX).
	call RandomRange ; Get random number for X and store in eax
	mov edx, eax     ; Keep the X number on standby in edx.
	
	mov eax, maxY
	call RandomRange ; Get random number for Y and store in eax

	;Check against food first
	mov edi, OFFSET foodArray
	mov ecx, numFood
	LFoodCheck:     ; Test Y against all elements of food

		mov bx, (COORD PTR [edi+(ecx*4)-4]).X
		cmp dx, bx
		jne OK3
	
		mov bx, (COORD PTR [edi+(ecx*4)-4]).Y
		cmp ax, bx
		jne OK3

		;else
		jmp RNGHarder

		OK3: ;Move to next index
	loop LFoodCheck

	mov edi, OFFSET snakeArray
	mov ecx, numSegmentsInPlay
	LSnakeCheck:

		mov bx, (COORD PTR [edi+(ecx*4)-4]).X
		cmp dx, bx
		jne OK4
	
		mov bx, (COORD PTR [edi+(ecx*4)-4]).Y
		cmp ax, bx
		jne OK4

		;else
		jmp RNGHarder

		OK4:
	loop LSnakeCheck
	;passed all tests, these two locations are good and stored in dx and ax respectively for use.

	ret
RegenerateFood ENDP

end main