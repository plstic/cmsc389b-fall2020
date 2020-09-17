print(((2+2 - 1)*8 / 2) // 3) -- math

do
    local a = 10
    b = 11
    print(a)
end
print(a)
print(b)

if b then
    print("b was not false and was not nil")
else
    print("b was false or nil")
end

-- some loops

i = 0
while i < 3 do
    print(i)
    i = i + 1
end

i = 0
repeat
    print(i)
    i = i + 1
until i > 2

for i=0,8,2 do
    print(i)
end
print(i) -- should be 3, from the previous loop
-- the variable created in a for-loop is local to the for-loop's scope only

for i=8,0,-1 do
    print(i)
end

a = {"a","b","c",key="val"}
for idx in pairs(a) do
    print(a[idx])
end

for k,v in ipairs(a) do
    print(k, v)
end

b = {key="val", key2="val2", key3=true}
for key in pairs(b) do
    print(key, b[key])
end
