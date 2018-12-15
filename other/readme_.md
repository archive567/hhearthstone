[hheartstone](https://github.com/tonyday567/hheartstone)
===

hsreplay phase
---


- [ ] align with https://hearthsim.info/hsreplay/
- [ ] dtd: https://github.com/HearthSim/HSReplay
- [ ] https://github.com/HearthSim/python-hsreplay
  - [ ] half parsed example: https://github.com/HearthSim/hsreplay-test-data/blob/master/hslog-tests/20457_broken_names.power.log
- [ ] https://hearthsim.info/docs/gamestate-protocol/
- [ ] xml
  - [ ] [xeno](https://hackage.haskell.org/package/xeno-0.3.4)
  - [ ] parse other/example.hsreplay
  - [ ] [xmlgen](http://hackage.haskell.org/package/xmlgen-0.6.2.2)
  - [ ] build a simple game fragment


joust
---

http-server --cors

stack build --exec "$(stack path --local-install-root)/bin/hheartstone" --exec "$(stack path --local-bin)/pandoc -f markdown -i other/header.md other/readme_.md other/footer.md -t html -o other/dist/index.html --filter pandoc-include --mathjax"

<div id="joust-container"></div>
<script type="text/javascript" src="https://cdnjs.cloudflare.com/ajax/libs/react/15.4.0/react.min.js"></script>
<script type="text/javascript" src="https://cdnjs.cloudflare.com/ajax/libs/react/15.4.0/react-dom.min.js"></script>
<link rel="stylesheet" href="joust.css"></link>
<script type="text/javascript" src="joust.js"></script>
<script type="text/javascript">
  Joust.launcher("joust-container")
    .height(500)
    .width(500)
    .fromUrl("http://192.168.0.2:8080/other/hsreplay/example.hsreplay");
</script>



test game state
---

```include
other/testGameState.md
```



notes
---

Modelling Heartstone

Gabrielle on [game simulation](http://www.haskellforall.com/2013/05/program-imperatively-using-haskell.html)

Game engine library (console not working in macos): [heartshroud](https://github.com/thomaseding/hearthshroud)

Get decks from hearthpwm: [blender](https://github.com/blender/Hearthstone)
Card manager: [HCM](https://github.com/nicuveo/HCM)
Simulator in python: [fireplace](https://github.com/jleclanche/fireplace)

Hearthstone AI [discussion](https://www.reddit.com/r/hearthstone/comments/7l1ob0/i_wrote_a_masters_thesis_on_effective_hearthstone/)

JSON of cards
---

[HearthstoneJSON](https://hearthstonejson.com/docs/cards.html)

[cards](https://api.hearthstonejson.com/v1/latest/enUS/cards.json)

test output
===

json stats
---

```include
other/json.md
```

workflow
===

```
stack build --exec "$(stack path --local-install-root)/bin/hheartstone" --exec "$(stack path --local-bin)/pandoc -f markdown -i other/readme_.md -t markdown -o readme.md --filter pandoc-include --mathjax"
```
