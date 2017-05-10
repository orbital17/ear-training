
compile: 
	elm-make src/Main.elm --output=dist.js --warn

watch:
	fswatch -o0rE src | xargs -0 -n1 -I{} make -s compile

chrome_reload: compile
	osascript -e 'tell application "Google Chrome" to tell the active tab of its first window to reload'

clean:
	rm -rf elm-stuff main.js

server:
	http-server