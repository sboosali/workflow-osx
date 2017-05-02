# only sets environment variables when undefined
# override with: (CC=gcc; make)
CC?=/usr/bin/gcc

run: main
	./main

main: main.m workflow.m
	$(CC) -ObjC  -framework Cocoa  -o ./main  -Wno-deprecated-declarations  $^
# launchedApplications is /usr/bin/ deprecated, but runningApplications doesn't return application info

clean:
	rm -f main
