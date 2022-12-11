lein run src/dactyl_keyboard/dactyl.clj
cp things/right.scad things/right.scad
cp things/left.scad things/left.scad
cp things/right-plate.scad things/right-plate.scad
openscad -o things/right-plate.dxf things/right-plate.scad >/dev/null 2>&1 &
openscad -o things/right.stl things/right.scad >/dev/null 2>&1 &
openscad -o things/left.stl  things/left.scad >/dev/null 2>&1 &

wait
