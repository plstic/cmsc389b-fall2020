-- you might find this helpful if you want to create custom snake paths for testing

local curses = require("curses")
-- make sure your terminal window is large enough
row, col = 12, 17
min_row, min_col, max_row, max_col = 0, 0, 25, 35
path = [[
wwddssaa
]]

exp_path = ""
for c in path:gmatch("[wasdq]%d*") do
    if #c > 1 then
        for i=1,tonumber(string.sub(c, 2)) do
            exp_path = exp_path .. string.sub(c, 1, 1)
        end
    else
        exp_path = exp_path .. c
    end
end

stdscr = curses.initscr() -- init screen
curses.cbreak() -- disables line buffering and erase/kill character-processing, making characters typed by the user immediately available to the program
curses.echo(false) -- do not echo characters to the terminal
curses.nl(false) -- do not translate return key to newline
curses.curs_set(0) -- invisible cursor
stdscr:nodelay(true) -- non-blocking getch()
stdscr:clear() -- clear screen

tbl = {
    w = function()
        row = row - 1
    end,
    a = function()
        col = col - 1
    end,
    s = function()
        row = row + 1
    end,
    d = function()
        col = col + 1
    end
}

-- https://stackoverflow.com/a/40524323
function sleep(a)
    local sec = tonumber(os.clock() + a);
    while (os.clock() < sec) do
    end 
end

for r = min_row,max_row do
    for c = min_col,max_col do
        stdscr:mvaddstr(r, c, ".")
    end
end

for c in exp_path:gmatch(".") do
    -- move
    stdscr:mvaddstr(row, col, "#")
    tbl[c]()
    -- update screen
    if string.char(stdscr:mvwinch(row, col)) ~= '.' then
        io.read() -- bad
    end
    stdscr:mvaddstr(row, col, "@")
    stdscr:refresh()
    -- wait
    sleep(0.03)
end

-- pause, end
io.read()
