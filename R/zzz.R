.onLoad <- function(libname,pkgname) {
    setDbPath()
}

#~ # Code for logo
#~
#~ library(ggplot2)
#~ library(rphylopic)
#~ library(hexSticker)
#~ library(showtext)

#~ human <- image_data("c089caae-43ef-4e4e-bf26-973dd4cb65c5",size=128)[[1]]
#~ mouse <- image_data("c4572239-3b7c-4259-8049-3f44e6d52e6f",size=128)[[1]]
#~ fruitfly <- image_data("ea8fa530-d856-4423-a81d-a74342cd1875",size=128)[[1]]
#~ zebra <- image_data("6f4c653a-1da1-4e02-85ef-6344b2d8e02a",size=128)[[1]]
#~ dino <- image_data("1f4746d0-42ed-4f85-a31f-447ca434caf5",size=128)[[1]]
#~ #pig: 008d6d88-d1be-470a-8c70-73625c3fb4fb
#~ pics <- list(human,mouse,fruitfly,zebra,dino)
#~ cols <- c("#073B4C","#118AB2","#06D6A0","#FFD166","#EF476F")
#~ sizes <- c(1.7,1,0.7,0.8,0.7)

#~ gd <- data.frame(
#~  x=c(0.7,1.5,2.5,4,4.2),
#~  y=c(1.2,4,1.5,4,1)
#~ )
#~ gt <- data.frame(
#~  x=2.5,
#~  y=2.7,
#~  z="italic('GAPDH')~~12:6534512-6538374"
#~ )

#~ font_add_google("Oswald","oswald")
#~ font_add_google("Titillium Web","titillium")
#~ font_add_google("Inconsolata","inconsolata")
#~ font_add_google("Bitter","bitter")
#~ font_add_google("Merriweather Sans","merriweather")

#~ showtext_auto()

#~ g <- ggplot(gd,aes(x=x,y=y)) + 
#~  xlim(0,5) + ylim(0,5) +
#~  geom_text(data=gt,aes(x=x,y=y,label=z),parse=TRUE,size=5,color="#BC6C25") +
#~  theme_void() + theme_transparent()
    
#~ for (i in 1:nrow(gd))
#~  g <- g + add_phylopic(pics[[i]],1,gd$x[i],gd$y[i],ysize=sizes[i],
#~      color=cols[i])

#~ sticker(g,package="sitadela",p_size=22,s_x=1,s_y=.75,s_width=1.3,s_height=1,
#~  p_color="#BC6C25",h_fill="#FEFAE0",h_color="#D4A373",
#~  p_family="merriweather",filename="test_sticker.png")
