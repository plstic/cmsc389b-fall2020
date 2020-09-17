s = "string"
i = 10
b = true
f = 5.0

print(s, i, b, f)

-- comment!

-- there only exist tables, no arrays
--  table : key -> value
--  use numeric keys as array indices

t = {}
t["key"] = "value"
print(t.key)

a = {"one","two",3,"four",6-1} -- "array"
t = {key="value", key2=2}

-- just prints the table pointer, not the contents!
print(a)
print(t)

-- see linked-list eg in func.lua
