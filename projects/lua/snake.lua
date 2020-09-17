-- we have to use the terminal as a screen
local curses = require("curses")

-- note: usually, you should use 'local' for variable declarations whenever possible
-- we need to test your code, however, so we're making some global

-- the following comment signifies code you should fill in:
-- *** --
-- as-in, your code should replace the "-- *** --"
-- ctrl+f is your friend here

-- current game state
Game = {
    Snake = {
        -- snake body data
        head = nil,
        tail = nil,
        -- snake velocity
        d_row = 0,
        d_col = 1,
        -- initialize the snake
        init = function(row, col)
            -- row, col == start position
            -- reset the head, tail, and velocity variables to their original state
            -- *** --
            -- then instantiate a structure that looks like so:
            --  tail = { next={ next=nil, row=row, col=col }, row=row-1, col=col }
            --                      ^
            --                      |
            --                      +--  = head
            -- (this should take multiple lines...)
            -- *** --
            -- finally, use the set_piece function in the Screen field to put a
            -- snake piece on the current row/col
            -- *** --
        end,
        -- velocities should satisfy:
        --  abs(d_row + d_col) = abs(d_row) + abs(d_col) = 1
        -- ie: only (1,0) (-1,0) (0,1) (0,-1)
        signal_up = function()
            -- switch the snake's velocity to go UP
            -- *** --
        end,
        signal_down = function()
            -- switch the snake's velocity to go DOWN
            -- *** --
        end,
        signal_left = function()
            -- switch the snake's velocity to go LEFT
            Game.Snake.d_row = 0
            Game.Snake.d_col = -1
        end,
        signal_right = function()
            -- switch the snake's velocity to go RIGHT
            -- *** --
        end,
        get_next_head_position = function()
            -- add the velocities to the current snake head row/col
            -- return the resulting row and col, as a tuple (row first, then col)
            -- *** --
        end,
        body_piece = function(row, col)
            -- snake structure hint hint ;)
            return {next = nil, row = row, col = col}
        end,
        extend = function()
            -- adds a new head onto the snake
            --
            -- instantiate a new snake body piece
            -- this body should be in the _next_ snake position
            --   calculate this by adding snake velocity to the current snake head's row/col
            -- set this new piece as the value of the current head's "next" field
            -- then set the current snake head to the current snake head's "next"
            -- ie, pictorally:
            -- head -> {next=nil, row=r, col=c}
            -- head -> {next={next=nil, row=r+d_r, col=c+d_c}, row=r, col=c}
            -- head -> {next=nil, row=r+d_r, col=c+d_c}
            -- *** --
        end,
        move = function(row, col)
            -- moves the snake to the given next position
            -- row,col == the position our snake is going to
            --
            -- this takes a few steps
            -- first, erase the current tail from the screen (set it to an empty piece)
            -- then, place down a snake piece on the board to where the current head is _going_ (row,col)
            -- *** --
            -- now is the cool part about snake
            --   we do the 'ol switcharoo
            -- first, set the current head's next piece to a new body (row/col as given in this func)
            -- then, set the current head to it's recently-assigned next piece
            -- finally, move the tail up by one by settting the current tail to it's next piece
            -- *** --
        end
    },
    Piece = {
        fruit = 'O',
        snake = '#',
        empty = ' ',
        unk = '?'
    },
    Screen = {
        -- screen boundary
        min_row = 0,
        min_col = 0,
        max_row = 0,
        max_col = 0,
        -- 2D array
        -- index by row, then by col (board[row] yields a list of elements)
        board = {},
        --
        init = function(min_row, min_col, max_row, max_col)
            -- first, set the min/max_row/col fields
            -- *** --
            -- then, instantiate the board by mapping every possible [row][col] to an empty game piece
            -- *** --
        end,
        middle = function()
            -- returns the rough midpoint of the Screen
            return (Game.Screen.max_row + Game.Screen.min_row)//2, (Game.Screen.max_col + Game.Screen.min_col)//2
        end,
        invalid_position = function(row, col)
            -- return true iff the given row/col is _outside_ of the Screen boundary
            -- the boundary is kept using the min/max_row/col fields
            -- *** --
        end,
        get_piece = function(row, col)
            -- validate the given row/col position
            -- then, if valid, return the piece inside the corresponding row/col in the Screen board
            -- if not valid, return the  unk  piece
            -- *** --
        end,
        set_piece = function(row, col, piece)
            -- validate the given row/col position
            -- then, if valid, set the board's row/col piece to the input piece
            -- *** --
            -- invalid positions need not do anything here
        end,
        check_collision = function(row, col)
            -- first, determine whether the given row/col collides with the screen boundary
            -- if it does, then call the hit_boundary field in Game
            -- *** --
            -- otherwise, retrieve the row/col game piece from the Screen
            -- and test:
            --   is it a fruit piece? yes -> call the consume_fruit field in Game
            --   is it a snake piece? yes -> call the eat_yourself field in Game
            -- *** --
        end
    },
    Fruit = {
        positions = {}, -- array of {row=, col=} tables
        curr_idx = 0, -- index into Game.Fruit.positions
        init = function()
            -- reset Fruit positions and curr_idx to their original standings
            -- *** --
            -- iterate through all possible ordered pairings of (Screen) row/col
            --  and add them into the local variable below
            -- the format is: {row= i, col= j} (for all i,j row/col pairs)
            local ordered_fruits = {}
            -- *** --
            -- randomly assign fruit positions into the fruit list
            for i, v in ipairs(ordered_fruits) do
                local pos = math.random(1, #Game.Fruit.positions+1)
                table.insert(Game.Fruit.positions, pos, v)
            end
            Game.Fruit.new_fruit()
        end,
        new_fruit = function()
            -- find the next empty fruit position
            -- do not include the next snake position
            for i = 1, #Game.Fruit.positions-1 do -- i == offset
                local f_pos = ((Game.Fruit.curr_idx + i) % #Game.Fruit.positions)+1
                local f_tbl = Game.Fruit.positions[f_pos]
                local c_pic = Game.Screen.get_piece(f_tbl.row, f_tbl.col)
                if c_pic == Game.Piece.empty then
                    r, c = Game.Snake.get_next_head_position()
                    if f_tbl.row ~= r and f_tbl.col ~= c then
                        Game.Fruit.curr_idx = f_pos
                        Game.Screen.set_piece(f_tbl.row, f_tbl.col, Game.Piece.fruit)
                        -- print(Game.Snake.head.row, Game.Snake.head.col, f_tbl.row, f_tbl.col)
                        return
                    end
                end
            end
            -- else, winner winner chicken dinner
            Game.win()
        end
    },
    interactive = nil,
    curr_path = "",
    orig_path = "",
    rpt = 0,
    quit = false,
    score = 0,
    end_message = "",
    -- initialize game state
    init = function(min_row, min_col, max_row, max_col)
        -- this is what i mean by "reset to the initial state"
        Game.curr_path = ""
        Game.orig_path = ""
        Game.rpt = 0
        Game.quit = false
        Game.score = 0
        Game.end_message = ""
        Game.Screen.init(min_row, min_col, max_row, max_col)
        Game.Snake.init(Game.Screen.middle())
        Game.Fruit.init()
    end,
    set_path = function(path)
        -- path \in wasdq#
        --  w4 represents wwww
        --  dw4d means dwwwwd (right, up x4, right)
        -- R# -> repeat path # times
        for c in path:gmatch("R%d+") do
            Game.rpt = tonumber(string.sub(c, 2))
        end
        for c in path:gmatch("[wasdq]%d*") do
            if #c > 1 then
                for i=1,tonumber(string.sub(c, 2)) do
                    Game.orig_path = Game.orig_path .. string.sub(c, 1, 1)
                end
            else
                Game.orig_path = Game.orig_path .. c
            end
        end
        Game.curr_path = Game.orig_path
    end,
    consume_fruit = function()
        -- player found a fruit!
        -- extend the snake head
        -- *** --
        -- increase the Game score by 1
        -- *** --
        -- place a new fruit (call Fruit new_fruit)
    end,
    hit_boundary = function()
        -- player ran into a boundary wall
        -- set the Game end_message to "YOU HIT THE BOUNDARY"
        -- *** --
        -- call signal_quit
        -- *** --
    end,
    eat_yourself = function()
        -- player ate themselves -- oof
        -- set the Game end_message to "YOU ATE YOURSELF"
        -- *** --
        -- call signal_quit
        -- *** --
    end,
    win = function()
        -- player won!
        -- set the Game end_message to "YOU WON!"
        -- *** --
        -- call signal_quit
        -- *** --
    end,
    signal_quit = function()
        -- signal end game
        Game.quit = true
    end,

    -- given!
    run = function(path)
        -- key-press lookup handler
        local c_tbl = {}
        local function dup_keys(keys, val)
            for _, v in ipairs(keys) do
                c_tbl[v] = val
            end
        end
        -- these values were obtained by trial/error lmao
        -- [arrow, WASD, IJKL]
        dup_keys({ 66, 115, 107, 's'}, "down")
        dup_keys({ 65, 119, 105, 'w'}, "up")
        dup_keys({ 67, 100, 108, 'd'}, "right")
        dup_keys({ 68,  97, 106, 'a'}, "left")
        dup_keys({113, 275,      'q'}     , "quit") -- [q, esc]
        -- user input -> action
        local function get_input()
            if Game.interactive then
                -- call the keyboard handler associated with the currently-pressed key
                return c_tbl[stdscr:getch()]
            else
                local c = string.sub(Game.curr_path, 1, 1)
                Game.curr_path = string.sub(Game.curr_path, 2)
                if #Game.curr_path < 1 then
                    Game.rpt = Game.rpt - 1
                    if Game.rpt < 1 then
                        Game.signal_quit()
                    end
                    Game.curr_path = Game.orig_path
                end
                return c_tbl[c] -- wasd/q
            end
        end
        -- action -> function
        local a_tbl = {
            down=Game.Snake.signal_down,
            up=Game.Snake.signal_up,
            right=Game.Snake.signal_right,
            left=Game.Snake.signal_left,
            quit=Game.signal_quit
        }
        -- interactive-mode timing info
        local prev_time = os.clock()
        local curr_time = os.clock()
        local decision_delay = 0.10 -- number of seconds to wait until advancing head


        -- initialize game
        local min_row, min_col, max_row, max_col = 0, 0, 25, 35 -- defaults
        if Game.interactive then
            -- interactive mode
            -- set up main curses screen
            stdscr = curses.initscr() -- init screen
            curses.cbreak() -- disables line buffering and erase/kill character-processing, making characters typed by the user immediately available to the program
            curses.echo(false) -- do not echo characters to the terminal
            curses.nl(false) -- do not translate return key to newline
            curses.curs_set(0) -- invisible cursor
            stdscr:nodelay(true) -- non-blocking getch()
            stdscr:clear() -- clear screen
            -- determine screen bounds
            min_row, min_col = stdscr:getbegyx()
            max_row, max_col = stdscr:getmaxyx()
            max_row = max_row - 1 -- reduce bottom padding
            max_col = max_col - 1 -- reduce right padding
            -- random seed math
            math.randomseed(os.time())
        end
        Game.init(min_row, min_col, max_row, max_col)
        Game.set_path(path)

        -- main loop
        -- we wrap the following code to facilitate error-catching/displaying
        local function main()
            -- main game loop
            repeat
                -- decide if we're accepting the current user input
                local do_update = true
                if Game.interactive then
                    -- quit if received
                    local a = get_input()
                    if a then
                        a_tbl[a]()
                    end
                    curr_time = os.clock()
                    if curr_time <= prev_time + decision_delay then
                        do_update = false
                    else
                        prev_time = curr_time
                    end
                end
                if do_update then
                    -- final action update
                    local a = get_input()
                    if a then
                        a_tbl[a]()
                    end
                    -- reset update
                    do_update = false
                    -- get next head position
                    row, col = Game.Snake.get_next_head_position()
                    -- check for collisions
                    Game.Screen.check_collision(row, col)
                    -- move entire snake forward
                    Game.Snake.move(row, col)
                    -- update display
                    if Game.interactive then
                        for r = Game.Screen.min_row,Game.Screen.max_row do
                            local row_str = ""
                            for c = Game.Screen.min_col,Game.Screen.max_col do
                                row_str = row_str .. Game.Screen.get_piece(r, c)
                            end
                            stdscr:mvaddstr(r, 0, row_str)
                        end
                        stdscr:refresh()
                    end
                end
            until Game.quit
            -- done
            if Game.interactive then
                curses.endwin()
                print(Game.end_message)
                print(string.format("Total score: %s", Game.score))
            end
        end

        -- To display Lua errors, we must close curses to return to
        -- normal terminal mode, and then write the error to stdout.
        local function error_handler(err)
            if Game.interactive then
                curses.endwin()
            end
            print("Caught an error:")
            print(debug.traceback (err, 2))
            os.exit(2)
        end

        xpcall(main, error_handler)
    end
}

-- lua snake.lua
if arg[0] == "snake.lua" then
    Game.interactive = true
    Game.run("")
end
