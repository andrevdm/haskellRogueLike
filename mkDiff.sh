#!/bin/sh

rm -rf _diff | true
rsync -mr --exclude '.git/***' --exclude '.stack-work' --exclude '_diff/***' --include '*/' --include '*hs' --include '*js' --include '*html' --include '*cabal' --exclude '*' ./[01]*_* ./_diff

find _diff/ -type f -name "*hs" | xargs sed -i 's/{-! SECTION.*//g'

cd _diff

#03
diff 01_web_ui/src/ 03_tiles/src/ -w -B -a -d -u -b --new-file > ../03_tiles/changes.patch | true

#04
diff 03_tiles/src/ 04_load_map/src/ -w -B -a -d -u -b --new-file > ../04_load_map/changes.patch | true

#05
diff 04_load_map/src/ 05_actors/src/ -w -B -a -d -u -b --new-file > ../05_actors/changes.patch | true

#06
diff 05_actors/src/ 06_moving/src/ -w -B -a -d -u -b --new-file > ../06_moving/changes.patch | true

#07
diff 06_moving/src/ 07_collisions/src/ -w -B -a -d -u -b --new-file > ../07_collisions/changes.patch | true

#08
diff 07_collisions/src/ 08_layers/src/ -w -B -a -d -u -b --new-file > ../08_layers/changes.patch | true
diff 07_collisions/html/rogue.html 08_layers/html/rogue.html -w -B -a -d -u -b >> ../08_layers/changes.patch | true
diff 07_collisions/html/rogue.js 08_layers/html/rogue.js -w -B -a -d -u -b >> ../08_layers/changes.patch | true

#09
diff 08_layers/src/ 09_viewport_scroll/src/ -w -B -a -d -u -b --new-file > ../09_viewport_scroll/changes.patch | true
diff 08_layers/html/rogue.html 09_viewport_scroll/html/rogue.html -w -B -a -d -u -b >> ../09_viewport_scroll/changes.patch | true
diff 08_layers/html/rogue.js 09_viewport_scroll/html/rogue.js -w -B -a -d -u -b >> ../09_viewport_scroll/changes.patch | true

#10
diff 09_viewport_scroll/src/ 10_fov/src/ -w -B -a -d -u -b --new-file > ../10_fov/changes.patch | true
diff 09_viewport_scroll/html/rogue.html 10_fov/html/rogue.html -w -B -a -d -u -b >> ../10_fov/changes.patch | true
diff 09_viewport_scroll/html/rogue.js 10_fov/html/rogue.js -w -B -a -d -u -b >> ../10_fov/changes.patch | true

#11
diff 10_fov/src/ 11_sticky_light/src/ -w -B -a -d -u -b --new-file > ../11_sticky_light/changes.patch | true
diff 10_fov/html/rogue.html 11_sticky_light/html/rogue.html -w -B -a -d -u -b >> ../11_sticky_light/changes.patch | true
diff 10_fov/html/rogue.js 11_sticky_light/html/rogue.js -w -B -a -d -u -b >> ../11_sticky_light/changes.patch | true

#12
diff 11_sticky_light/src/ 12_energy/src/ -w -B -a -d -u -b --new-file > ../12_energy/changes.patch | true
diff 11_sticky_light/html/rogue.html 12_energy/html/rogue.html -w -B -a -d -u -b >> ../12_energy/changes.patch | true
diff 11_sticky_light/html/rogue.js 12_energy/html/rogue.js -w -B -a -d -u -b >> ../12_energy/changes.patch | true

#13
diff 12_energy/src/ 13_utility/src/ -w -B -a -d -u -b --new-file > ../13_utility/changes.patch | true
diff 12_energy/html/rogue.html 13_utility/html/rogue.html -w -B -a -d -u -b >> ../13_utility/changes.patch | true
diff 12_energy/html/rogue.js 13_utility/html/rogue.js -w -B -a -d -u -b >> ../13_utility/changes.patch | true

#14
diff 13_utility/src/ 14_utility_annotate/src/ -w -B -a -d -u -b --new-file > ../14_utility_annotate/changes.patch | true
diff 13_utility/html/rogue.html 14_utility_annotate/html/rogue.html -w -B -a -d -u -b >> ../14_utility_annotate/changes.patch | true
diff 13_utility/html/rogue.js 14_utility_annotate/html/rogue.js -w -B -a -d -u -b >> ../14_utility_annotate/changes.patch | true

#15
diff 14_utility_annotate/src/ 15_memory/src/ -w -B -a -d -u -b --new-file > ../15_memory/changes.patch | true
diff 14_utility_annotate/html/rogue.html 15_memory/html/rogue.html -w -B -a -d -u -b >> ../15_memory/changes.patch | true
diff 14_utility_annotate/html/rogue.js 15_memory/html/rogue.js -w -B -a -d -u -b >> ../15_memory/changes.patch | true

#16
diff 15_memory/src/ 16_debug/src/ -w -B -a -d -u -b --new-file > ../16_debug/changes.patch | true
diff 15_memory/html/rogue.html 16_debug/html/rogue.html -w -B -a -d -u -b >> ../16_debug/changes.patch | true
diff 15_memory/html/rogue.js 16_debug/html/rogue.js -w -B -a -d -u -b >> ../16_debug/changes.patch | true

#17
diff 16_debug/src/ 17_levels/src/ -w -B -a -d -u -b -r --new-file > ../17_levels/changes.patch | true
diff 16_debug/app/ 17_levels/app/ -w -B -a -d -u -b -r --new-file >> ../17_levels/changes.patch | true
diff 16_debug/html/rogue.html 17_levels/html/rogue.html -w -B -a -d -u -b >> ../17_levels/changes.patch | true
diff 16_debug/html/rogue.js 17_levels/html/rogue.js -w -B -a -d -u -b >> ../17_levels/changes.patch | true

#18
diff 17_levels/src/ 18_multi_level/src/ -w -B -a -d -u -b -r --new-file > ../18_multi_level/changes.patch | true
diff 17_levels/app/ 18_multi_level/app/ -w -B -a -d -u -b -r --new-file >> ../18_multi_level/changes.patch | true
diff 17_levels/html/rogue.html 18_multi_level/html/rogue.html -w -B -a -d -u -b >> ../18_multi_level/changes.patch | true
diff 17_levels/html/rogue.js 18_multi_level/html/rogue.js -w -B -a -d -u -b >> ../18_multi_level/changes.patch | true

#19
diff 18_multi_level/src/ 19_story/src/ -w -B -a -d -u -b -r --new-file > ../19_story/changes.patch | true
diff 18_multi_level/app/ 19_story/app/ -w -B -a -d -u -b -r --new-file >> ../19_story/changes.patch | true
diff 18_multi_level/html/rogue.html 19_story/html/rogue.html -w -B -a -d -u -b >> ../19_story/changes.patch | true
diff 18_multi_level/html/rogue.js 19_story/html/rogue.js -w -B -a -d -u -b >> ../19_story/changes.patch | true

cd ..


cat 03_tiles/changes.patch            | ./diff2html > 03_tiles/changes.patch.html
cat 04_load_map/changes.patch         | ./diff2html > 04_load_map/changes.patch.html
cat 05_actors/changes.patch           | ./diff2html > 05_actors/changes.patch.html
cat 06_moving/changes.patch           | ./diff2html > 06_moving/changes.patch.html
cat 07_collisions/changes.patch       | ./diff2html > 07_collisions/changes.patch.html
cat 08_layers/changes.patch           | ./diff2html > 08_layers/changes.patch.html
cat 09_viewport_scroll/changes.patch  | ./diff2html > 09_viewport_scroll/changes.patch.html
cat 10_fov/changes.patch              | ./diff2html > 10_fov/changes.patch.html
cat 11_sticky_light/changes.patch     | ./diff2html > 11_sticky_light/changes.patch.html
cat 12_energy/changes.patch           | ./diff2html > 12_energy/changes.patch.html
cat 13_utility/changes.patch          | ./diff2html > 13_utility/changes.patch.html
cat 14_utility_annotate/changes.patch | ./diff2html > 14_utility_annotate/changes.patch.html
cat 15_memory/changes.patch           | ./diff2html > 15_memory/changes.patch.html
cat 16_debug/changes.patch            | ./diff2html > 16_debug/changes.patch.html
cat 17_levels/changes.patch           | ./diff2html > 17_levels/changes.patch.html
cat 18_multi_level/changes.patch      | ./diff2html > 18_multi_level/changes.patch.html
cat 19_story/changes.patch            | ./diff2html > 19_story/changes.patch.html
