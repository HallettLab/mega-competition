

library(lterpalettefinder)

test2 <- palette_extract(image = "color_palette_tests/IMG_2730.jpg", progress_bar = FALSE)
test2

"#c19352" "#514b15" "#74422f" "#a9725e" "#c08e7a" "#905a43" "#a28483" "#d3b7b2" "#d7ac6e" "#4f2a18" "#f2be36"
[12] "#f2cf7a" "#e6cec9" "#f7eadd" "#837d44" "#896d69" "#6e6628" "#564237" "#d3c192" "#bf9e9b" "#705751" "#b3af77"
[23] "#9a955c" "#ac7835" "#f0dbaa"

"#be905e" "#b5b279" "#4d2a18" "#a46c57" "#827c42" "#f7eade" "#dfc88e" "#af8c88" "#ae7c36" "#69534a"
[11] "#6c6326" "#623c31" "#d5bbb1" "#d0a28a" "#9a965c" "#8a563b" "#816561" "#4f4a17" "#9b7977" "#bea5a2"
[21] "#f2deab" "#d9ae69" "#f9d34b" "#e6ceca" "#eaac2f"


test3 <- palette_extract(image = "color_palette_tests/IMG_2732.jpg", progress_bar = FALSE)
test3

"#edc6a9" "#f2d788" "#535921" "#f6cb4a" "#f2ecde" "#7b6954" "#744e38" "#e6ad88" "#4d361c" "#b3ba7d"
[11] "#a0a453" "#d2de68" "#a39b8a" "#bfb3a5" "#6d752e" "#b3c252" "#d6d0c5" "#878f3c" "#aa714d" "#d9ac68"
[21] "#edebb5" "#8d866c" "#bf8d6f" "#d5924b" "#ced694"


test4 <- palette_extract(image = "color_palette_tests/IMG_3469.jpg", progress_bar = FALSE)
test4

"#9d926a" "#8e8256" "#73776f" "#b5a67a" "#6b674c" "#4a3609" "#88763e" "#ceb367" "#60562d" "#5c4910"
[11] "#c4cccf" "#eaece3" "#e5ca78" "#4f5141" "#7b682f" "#e1cf9d" "#b59d57" "#ccbb8a" "#f6e18c" "#70591d"
[21] "#f7e9b2" "#a08845" "#4b4421" "#a5adaf" "#888f8f"

"#f8e490" "#f8e9b4" "#7c7149" "#a1a8aa" "#4e492c" "#d0c08d" "#5d5e4e" "#aa934f" "#bcad7e" "#a79a6d"
[11] "#c1a75e" "#eacf7c" "#747771" "#806a2c" "#695d31" "#92865d" "#e4d2a0" "#b8c0c3" "#d6bb6c" "#4d3b0c"
[21] "#8b908d" "#d1d8da" "#957f3f" "#655118" "#f1f1e4"

test5 <- palette_extract(image = "color_palette_tests/IMG_3094.jpg", progress_bar = FALSE)
test5

"#886135" "#cd5932" "#b67149" "#d2cd86" "#997a4b" "#b5be64" "#ba9261" "#9ba44e" "#636b24" "#b1af8e"
[11] "#7f8a39" "#6c6b4c" "#df7f5d" "#b47c20" "#838563" "#ecd9a2" "#dea936" "#ccc5aa" "#9b9b78" "#4f5136"
[21] "#684a24" "#d2ab7a" "#474213" "#f1e8ce" "#ab4512"


test6 <- palette_extract(image = "color_palette_tests/IMG_3500.jpg", progress_bar = FALSE)
test6

"#e3e8ef" "#d65cc3" "#9ba86f" "#66484c" "#b64c9e" "#9f9a97" "#ae8a77" "#4d2e36" "#4c5429" "#b9b48c"
[11] "#b67baa" "#90665b" "#e5a8eb" "#828c56" "#e87ce4" "#913a73" "#cec9a8" "#cbc6d1" "#cb93ca" "#9b6688"
[21] "#687140" "#87837a" "#725f6a" "#b6aeb1" "#e4e2c6"

test7 <- palette_extract(image = "color_palette_tests/IMG_3370.jpg", progress_bar = FALSE)
test7

"#4b371e" "#bd7427" "#e27519" "#c9b397" "#be5c12" "#988265" "#a69176" "#d79226" "#b7a285" "#5a472d"
[11] "#887457" "#996235" "#cd8640" "#e4b78b" "#78654a" "#ac774a" "#661c02" "#f19d2c" "#894d1d" "#bf8c62"
[21] "#d2a077" "#e0c8ac" "#e0a24c" "#f5e1c7" "#69563c"

test8 <- palette_extract(image = "color_palette_tests/IMG_3061.jpg", progress_bar = FALSE)
test8

"#dbb08a" "#f6f2e2" "#cec09f" "#494428" "#938878" "#e2d1af" "#bcac86" "#b9b0a2" "#a59c8e" "#a99b74"
[11] "#cec7bb" "#7d7a48" "#ca9e6a" "#9e784b" "#89643d" "#827564" "#4a3511" "#59523e" "#6d6351" "#705d32"
[21] "#634923" "#b2885e" "#948c5d" "#e7dfcb" "#eecf71"

test9 <- palette_extract(image = "color_palette_tests/IMG_3458.jpg", progress_bar = FALSE)
test9

"#d4bde2" "#b99dd8" "#f1dd4a" "#62553a" "#eae386" "#504218" "#9e8b92" "#d8ca9d" "#bfb1ba" "#a5a45d"
[11] "#b5ad8f" "#c4c06e" "#b7966f" "#96865a" "#837277" "#7e7047" "#ede3eb" "#473649" "#99872b" "#9d8abe"
[21] "#7b6fa2" "#63536f" "#73671d" "#ece3ba" "#d7b182"

test10 <- palette_extract(image = "color_palette_tests/IMG_2805.jpg", progress_bar = FALSE)
test10

"#8b553a" "#b375a1" "#511d13" "#aea954" "#8c8f34" "#733333" "#766e50" "#f0dfac" "#aa6557" "#531938"
[11] "#e5ad96" "#926384" "#938b62" "#d5ccb1" "#f3e0e1" "#e6b2d6" "#a3958a" "#574544" "#676e1e" "#ccc175"
[21] "#c68972" "#7c4565" "#4d4916" "#c896b9" "#bbb894"
