---
-- public tests
---

-- test hello
do
    f = io.popen(("lua hello.lua"))
    assert(f:read() == "Hello, World!")
    print("testHello passed")
    f:close()
end

-- test snake
-- import student code
require('snake') -- non-interactive
math.randomseed(12345)

-- check basic initializations
do
    Game.init(20, 30, 40, 50)
    --
    assert(Game.curr_path == "")
    assert(Game.orig_path == "")
    assert(Game.rpt == 0)
    assert(Game.quit == false)
    assert(Game.score == 0)
    assert(Game.end_message == "")
    print("testBasicInit passed")
end

-- remember, calling .run() should entirely reset the game

-- quick quit
do
    Game.run("dq")
    assert(Game.end_message == "")
    assert(Game.quit == true)
    assert(Game.score == 0)
    print("testQuickQuit passed")
end

-- eat yo-self
do
    Game.run("da")
    assert(Game.end_message == "YOU ATE YOURSELF")
    print("testEatYoSelf passed")
end

-- wall time
do
    -- 17 -> 35
    Game.run("d18w") -- should not collide
    assert(Game.end_message == "")
    assert(Game.quit == true)
    assert(Game.score == 0)
    Game.run("d19w") -- should collide
    assert(Game.end_message == "YOU HIT THE BOUNDARY")
    assert(Game.quit == true)
    assert(Game.score == 0)
    print("testWallTime passed")
end

-- small loop
do
    Game.run("R10ddwwaass")
    assert(Game.end_message == "")
    assert(Game.quit == true)
    assert(Game.score == 0)
    -- from default mid position (12,17)->
    assert(Game.Snake.head.row == 12)
    assert(Game.Snake.head.col == 17)
    assert(Game.Snake.tail.row == 11)
    assert(Game.Snake.tail.col == 17)
    print("testSmallLoop passed")
end

-- end public tests
