-- functions are first-class

function add5(num)
    return num + 5
end

-- https://stackoverflow.com/a/12373551
function tail(t)
    local function helper(head, ...) return #{...} > 0 and {...} or nil end
    return helper((table.unpack or unpack)(t))
end

function map(lst, func)
    if lst then
        print(func(lst[1]))
        map(tail(lst), func)
    end
end

map({5, 8, 4, -5, 2, 0}, add5)

-- so we have functinal elements in Lua
-- but the main purpose isn't "functional"
--  as you can see, concatenating lists together isn't straightforward
--  (b/c lists are hash-tables, not linked-lists!)

-- linked-list eg:
-- head = {next=nil, data=nil}
function add(node, data)
    if node == nil then
        return {next=nil, data=data}
    else
        node.next = add(node.next, data)
        return node
    end
end

head = add(add(add(nil, 30), 20), 10)

n = head
while n ~= nil do
    print(n.data)
    n = n.next
end
