{
  greeter = name: "Hello, ${name}!";
  
  greetTimes = name: count:
    builtins.genList (i: greeter name) count;
  
  config = {
    message = greeter "World";
    times = 3;
  };
}

