#! usr/local/bin/io

// 1.
// loop
fib := method(n,
    if(n == 0, return 0)
    if(n == 1, return 1)
    count := 1
    prev := 0
    current := 1
    while(n > count,
        new := prev + current
        prev = current
        current = new
        count = count + 1
    )
    return current
)
fib(4) println

// nicer
fib := method(num,
    prev := 0
    current := 1
    for(i, num, 1, -1,
        next := prev + current
        prev = current
        current = next
    )
    prev
)
fib(4) println

// recursive
fib := method(n,
    if(n == 0, return 0)
    if(n == 1, return 1)
    return fib(n-1) + fib(n-2)
)
fib(4) println

// nicer
fib := method(n, if(n <= 1, n, fib(n-1) + fib(n-2)))
fib(4) println


// 2.
Number origDiv := Number getSlot("/")
Number / := method(denom,
    if(denom == 0, 0, self origDiv(denom))
)
5 / 0 println


// 3.
sum := method(array,
    sum := 0
    array foreach(i, dim2,
        dim2 foreach(j, v, sum = sum + v)
    )
    sum
)
l := list(list(3, 4), list(1, 2))
sum(l) println

//nicer
sum := method(array, array flatten sum)
sum(l) println


// 4.
List myAvg := method(self average)
l := list(2, 3)
l myAvg println

// bonus - it does not work with Exception handling as there is no such exception
//          like "item in list is not a number" which could be caught
// This approach however won't work for descendants of Number
List myAvg := method(
    containsNonDigit := select(x, x type != "Number") size > 0
    if(containsNonDigit, Exception raise("An item in the list is not a number!"))
    flatList := self flatten
    flatList average
)
//l := list(1, "2", "3")
l myAvg println

// better
List myAvg := method(
    containsNonDigit := select(x, x asNumber() isNan()) size > 0
    if(containsNonDigit, Exception raise("An item in the list is not a number!"))
    flatList := self map(asNumber()) flatten
    flatList average
)
l := list(2, "3")
l myAvg println


// 5.
List2D := List clone
List2D dim := method(x, y,
    self removeAll()
    y repeat( 
        // fancy: self append(Range 1 to(x) asList() map(nil))
        tmp := list()
        x repeat(tmp append(nil))
        self append(tmp)
    )
)
List2D dim(2, 3)
List2D println

List2D set := method(x, y, value, self at(y) atPut(x, value))
List2D set(1, 2, "oh yeah!")
List2D println

List2D get := method(x, y, self at(y) at(x))
List2D get(1, 2) println


// 6.
List2D transpose := method(
    transposedList := List2D clone dim(self size, self at(0) size)
    self foreach(y, subList, 
        subList foreach(x, value,
            transposedList set(y, x, value)
        )
    )
    transposedList
)
List2D transpose println


// 7.
List2D toFile := method(name,
    File with(name) open write(self serialized) close
)

List2D fromFile := method(name,
    doRelativeFile(name)
)


// 8.
random := Random value(100) ceil
last_guess := nil
for (i, 1, 11,
    if(i == 11, "You lose!! It was " .. random .. "!" println break)
    ("Guess a number between 1 and 100 (guess " .. i .. " of 10): ") print
    guess := ReadLine readLine asNumber()
    if(guess == random,
        "Wow! You got it right!" println break,
        "Nope...try again" println
        if(last_guess isNil() not,
            if((guess - random) abs < last_guess,
                "hotter" println,
                "colder" println
            )
        )
        last_guess = (guess - random) abs
    )
)
