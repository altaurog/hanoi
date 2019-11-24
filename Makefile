CLOSURE_FLAGS=--language_in ECMASCRIPT5 --compilation_level ADVANCED_OPTIMIZATIONS --warning_level QUIET
all: hanoi.min.js

clean:
	$(RM) hanoi.js hanoi.min.js

hanoi.js: src/Main.elm src/HanoiView.elm
	yarn run elm make --output=$@ --optimize $<

%.min.js: %.js
	closure-compiler $(CLOSURE_FLAGS) --js $< > $@
