[hheartstone](https://github.com/tonyday567/hheartstone)
===

Modelling Heartstone

Gabrielle on [game simulation](http://www.haskellforall.com/2013/05/program-imperatively-using-haskell.html)

Game engine library (console not working in macos): [heartshroud](https://github.com/thomaseding/hearthshroud)

Get decks from hearthpwm: [blender](https://github.com/blender/Hearthstone)
Card manager: [HCM](https://github.com/nicuveo/HCM)
Simulator in python: [fireplace](https://github.com/jleclanche/fireplace)

Hearthstone AI [discussion](https://www.reddit.com/r/hearthstone/comments/7l1ob0/i_wrote_a_masters_thesis_on_effective_hearthstone/)

api
---

```include
other/api.md
```

JSON of cards
---

[HearthstoneJSON](https://hearthstonejson.com/docs/cards.html)

[cards](https://api.hearthstonejson.com/v1/latest/enUS/cards.json)

output
---

```include
other/json.md
```

workflow
===

```
stack build --exec "$(stack path --local-install-root)/bin/hheartstone" --exec "$(stack path --local-bin)/pandoc -f markdown+lhs -i src/Hearth.lhs -t markdown -o other/api.md --filter pandoc-include --mathjax" --exec "$(stack path --local-bin)/pandoc -f markdown -i other/readme_.md -t markdown -o readme.md --filter pandoc-include --mathjax" --file-watch
```
