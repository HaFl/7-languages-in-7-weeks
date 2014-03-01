#! usr/local/bin/io

/* Good to know
- the special thing about operators (which are syntactic sugar for functions or
    in the case of Io messages) is that you don't need to write 5 /(4) but
    instead can write 5 / 4
- spacing is weird in Io as you basically could write everything on one line
    (due to everything being a message). However, the use of ";" is possible
- method overloading might be a problem in Io due to the syntax --> how do I
    know that writeln "slowly" is different from writeln("slowly")
- documentation is really bad! While "... slotNames sort" helps to find out
    the functions, there seems to be no way of telling what input they expect
    or to get a simple description of them. Also, where are special functions
    like curlyBrackets, foreach, call message arguments etc. documented?
    Maybe I just didn't find the best documentation on the Internet yet...
- debugging is a pain: whenever one copies from the editor to the REPL, the
    indentation is screwed up and has to be straigthened out with ";"s.
    Additionally, for my last task, the script behaved different in the REPL
    than when called from file!
- the fact that I had to add a hack to the source code so that I would be able
    to install it on my MacBook alone tells me that the language is not used
    a lot
*/

// 1. enhance the XML program
Builder := Object clone
Builder indent := 0
Builder forward := method(
    self indent repeat(write(" "))
    writeln("<", call message name, ">")
    self indent = self indent + 4
    call message arguments foreach(
        arg,
        content := self doMessage(arg);
        if(content type == "Sequence",
            self indent repeat(write(" "))
            writeln(content))
    )
    self indent = self indent - 4
    self indent repeat(write(" "))
    writeln("</", call message name, ">")
)

Builder ul(
    li("Io"),
    li("Lua"),
    li("JavaScript")
)

// nicer
Builder indentLevel := 0
Builder indent := method(
    spaces := ""
    indentLevel repeat(spaces = spaces .. "    ")
    spaces
)
Builder forward := method(
    writeln(indent .. "<", call message name, ">")
    indentLevel = indentLevel + 1
    call message arguments foreach(
        arg,
        content := self doMessage(arg);
        if(content type == "Sequence",
            writeln(indent .. content))
    )
    indentLevel = indentLevel - 1
    writeln(indent .. "</", call message name, ">")
)

Builder ul(
    li("Io"),
    li("Lua"),
    li("JavaScript")
)


// 2. brackets for list
squareBrackets := method(
    call message arguments
)
[1, 2, 3]


// 3. XML program which handles attributes
// I have no idea why: this works in the REPL, but doesn't when called from file
OperatorTable addAssignOperator(":", "convertToAttr")
convertToAttr := method(
    " " ..
    (call evalArgAt(0) asMutable removePrefix("\"") removeSuffix("\"")) ..
    "=" ..
    "\"" ..
    (call evalArgAt(1)) ..
    "\""
)
Builder curlyBrackets := method(
    attr := ""
    call message arguments foreach(
        arg,
        attr = attr .. doMessage(arg)
    )
    attr
)
Builder forward := method(
    write(indent .. "<" .. call message name)
    indentLevel = indentLevel + 1
    call message arguments foreach(
        i,
        arg,
        if(i == 0 and arg name == "curlyBrackets",
            writeln(self doMessage(arg) .. ">"),
            if(i == 0, writeln(">"))
            content := self doMessage(arg)
            if(content type == "Sequence", writeln(indent .. content))
        )
    )
    indentLevel = indentLevel - 1
    writeln(indent .. "</", call message name, ">")
)

Builder ul(
    {"what":"languages"},
    li("Io"),
    li("Lua"),
    li("JavaScript")
)
