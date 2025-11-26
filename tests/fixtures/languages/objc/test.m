#import <Foundation/Foundation.h>

@interface Greeter : NSObject
@property (nonatomic, strong) NSString *greeting;
- (void)greet:(NSString *)name;
- (void)greetTimes:(NSString *)name count:(int)count;
@end

@implementation Greeter

- (instancetype)init {
    self = [super init];
    if (self) {
        _greeting = @"Hello";
    }
    return self;
}

- (void)greet:(NSString *)name {
    NSLog(@"%@, %@!", self.greeting, name);
}

- (void)greetTimes:(NSString *)name count:(int)count {
    for (int i = 0; i < count; i++) {
        [self greet:name];
    }
}

@end

int main(int argc, const char * argv[]) {
    @autoreleasepool {
        Greeter *greeter = [[Greeter alloc] init];
        [greeter greetTimes:@"World" count:3];
    }
    return 0;
}

