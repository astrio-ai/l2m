namespace eval Greeter {
    variable greeting "Hello"
    
    proc greet {name} {
        variable greeting
        puts "$greeting, $name!"
    }
    
    proc greet_times {name count} {
        for {set i 0} {$i < $count} {incr i} {
            greet $name
        }
    }
}

Greeter::greet_times "World" 3

