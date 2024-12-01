library(leaflet)
library(shiny)
library(shinythemes)
library(shinydashboard)
library(shinyauthr)
library(googledrive)
library(readxl)
library(ggplot2)
library(plotly)
library(dplyr)
library(stringr)
library(rgdal)
library(sp)
library(reactable)
library(formattable)
library(sparkline)
library(reactable)
library(htmltools)
library(xlsx)
library(klaR)

user_base<-tibble::tibble(
  user = c("admin","user1"),
  password = sapply(c("admin123","user111110"), sodium::password_store),
  permissions = c("admin","standard"),
  name = c("admin","user")
)



rating_column <- function(maxWidth = 55, ...) {
  colDef(maxWidth = maxWidth, align = "center", class = "cell number", ...)
}

group_column <- function(class = NULL, ...) {
  colDef(cell = format_pct, maxWidth = 70, align = "center", class = paste("cell number", class), ...)
}

knockout_column <- function(maxWidth = 70, class = NULL, ...) {
  colDef(
    cell = format_pct,
    maxWidth = maxWidth,
    class = paste("cell number", class),
    style = function(value) {
      # Lighter color for <1%
      if (value < 0.01) {
        list(color = "#aaa")
      } else {
        list(color = "#111", background = knockout_pct_color(value))
      }
    },
    ...
  )
}

knockout_column2 <- function(maxWidth = 70, class = NULL, ...) {
  colDef(
    cell = format_pct,
    maxWidth = maxWidth,
    class = paste("cell number", class),
    style = function(value) {
      # Lighter color for <1%
      if (value < 0.01) {
        list(color = "#aaa")
      } else {
        list(color = "#111", background = knockout_pct_color2(value))
      }
    },
    ...
  )
}

format_pct <- function(value) {
  if (value == 0) "  \u2013 "    # en dash for 0%
  else if (value == 1) "\u2713"  # checkmark for 100%
  else if (value < 0.01) " <1%"
  else if (value > 0.99) ">99%"
  else formatC(paste0(round(value * 100), "%"), width = 4)
}

make_color_pal <- function(colors, bias = 1) {
  get_color <- colorRamp(colors, bias = bias)
  function(x) rgb(get_color(x), maxColorValue = 255)
}

off_rating_color <- make_color_pal(c("#44ab43", "#f8fcf8", "#ff2700"), bias = 1.3)
def_rating_color <- make_color_pal(c("#ff2700", "#f8fcf8", "#44ab43"), bias = 0.6)
knockout_pct_color <- make_color_pal(c("#ffffff", "#f2fbd2", "#c9ecb4", "#93d3ab", "#35b0ab"), bias = 2)
knockout_pct_color2 <- make_color_pal(c("#35b0ab", "#93d3ab", "#c9ecb4","#f2fbd2" ,"#ffffff" ), bias = 2)


#setwd("/Users/siwachoat/Desktop/งาน กสศ. เต๊าะ")
## ข้อมูลสุขภาพนักเรียน

#drive_download("https://docs.google.com/spreadsheets/d/159ebdxI1eeyI_Xk1qu_e3tZNy_mJ-VHAWorzviUekBM/edit?usp=sharing")

#dat<-read_excel("ข้อมูลแบบสอบถามสุขภาพ.xlsx")
#names(dat)[1]<-"school.name"
#names(dat)[2]<-"level"
#names(dat)[6]<-"student.id"
#names(dat)[7:9]<-c("eye1","eye2","eye3")
#names(dat)[10:12]<-paste("ear",1:3,sep="")
#names(dat)[13:15]<-paste("mouth",1:3,sep="")
#names(dat)[16:19]<-paste("skin",1:4,sep="")
#names(dat)[20:23]<-paste("digest",1:4,sep="")
#names(dat)[24:27]<-paste("nutrition",1:4,sep="")
#names(dat)[28:31]<-paste("move",1:4,sep="")
#names(dat)[32:37]<-paste("emo",1:6,sep="")
#names(dat)[38:40]<-paste("hygiene",1:3,sep="")



dat<-read_excel("ข้อมูลแบบสอบถามสุขภาพ_ใช้จริง.xlsx")
names(dat)<-c("school.name", "level","room",
              "name","surname","student.id",
              paste("eye",1:3,sep=""),
              paste("ear",1:3,sep=""),
              paste("mouth",1:3,sep=""),
              paste("skin",1:4,sep=""),
              paste("digest",1:4,sep=""),
              paste("nutrition",1:4,sep=""),
              paste("move",1:4,sep=""),
              paste("emo",1:6,sep=""),
              paste("hygiene",1:3,sep=""),
              paste("order",1:3,sep=""),
              paste("bev",1:10,sep=""))


dat$id<-1:dim(dat)[1]
dat%>%filter(school.name=="โรงเรียนตำรวจตะเวนชายแดนบำรุงที่ 87")%>%data.frame()
dat[4057,1]<-"โรงเรียนตชด.ที่ 87"
names(dat)

dat<-dat%>%dplyr::select(id, school.name, level, room, name, surname, student.id,
                          eye1, eye2,
                          ear1, ear2,
                          mouth1, mouth2,
                          skin1, skin2, skin3,
                          digest1, digest2, digest3,
                          nutrition1, nutrition2, nutrition3,
                          move1, move2, move3,
                          hygiene1, hygiene2)%>%mutate_at(vars(8:27), ~(ifelse(.==1,0,1)))


dat2<-dat%>%mutate(eye=eye1+eye2)#, eye=ifelse(eye>2,1,0))
dat2<-dat2%>%mutate(ear=ear1+ear2)#, ear=ifelse(ear>2,1,0))
dat2<-dat2%>%mutate(mouth=mouth1+mouth2)#, mouth=ifelse(mouth>2,1,0))
dat2<-dat2%>%mutate(skin=skin1+skin2+skin3)#, skin=ifelse(skin>3,1,0))
dat2<-dat2%>%mutate(digest=digest1+digest2+digest3)#, digest=ifelse(digest>3,1,0))
dat2<-dat2%>%mutate(nutrition=nutrition1+nutrition2+nutrition3)#, nutrition=ifelse(nutrition>3,1,0))
dat2<-dat2%>%mutate(move=move1+move2+move3)#,  move=ifelse(move>3,1,0))
dat2<-dat2%>%mutate(hygiene=hygiene1+hygiene2)#,  hygiene=ifelse(hygiene>2,1,0))
head(dat2)

#dat2$school.name[dat2$school.name=="โรงเรียนตำรวจตะเวนชายแดนบำรุงที่ 87"]<-"โรงเรียนตชด.ที่ 87"

#names(dat2)[4]<-"student.name"
problem.ind<-dat2%>%mutate(sum.problem=eye+ear+mouth+skin+digest+nutrition+move+hygiene)
problem.ind<-problem.ind%>%filter(sum.problem>0)
#problem.ind<-problem.ind%>%dplyr::select(-eye3, -ear3, -mouth3, -skin4, -digest4, -nutrition4,
 #                                        -move4, -emo1, -emo2, -emo3, -emo4, -emo5, -emo6,
  #                                       -hygiene3,
   #                                      -41:-61)




dat2<-dat2%>%filter(is.na(school.name)==FALSE)%>%
  group_by(school.name)%>%
  summarise(eye=sum(eye),
            ear=sum(ear),
            mouth=sum(mouth),
            skin=sum(skin),
            digest=sum(digest),
            nutrition=sum(nutrition),
            move=sum(move),
            hygiene=sum(hygiene),
            total=n())
dat2[7,1]<-"โรงเรียนบ้านปางมะหัน"
dat2[8,1]<-"โรงเรียนบ้านพญาไพร"


dat3<-dat%>%mutate(level=factor(level, 
                                levels=c(paste("อ.",2:3, sep=""), paste("ป.", 1:6,sep=""), paste("ม.",1:3,sep=""))))


schooldata<-problem.ind%>%filter(is.na(school.name)==FALSE)%>%
  group_by(school.name)%>%
  summarise(num.problem=list(sum.problem),
            mean.problem=mean(sum.problem),
            min.problem=min(sum.problem),
            max.problem=max(sum.problem),
            eye=sum(eye1+eye2),
            ear=sum(ear1+ear2),
            mouth=sum(mouth1+mouth2),
            skin=sum(skin1+skin2+skin3),
            digest=sum(digest1+digest2+digest3),
            nutrition=sum(nutrition1+nutrition2+nutrition3),
            move=sum(move1+move2+move3),
            hygiene=sum(hygiene1, hygiene2))
schooldata[7,1]<-"โรงเรียนบ้านปางมะหัน"
schooldata[8,1]<-"โรงเรียนบ้านพญาไพร"


### --- DMC

#dmc<-read_excel("/Users/siwachoat/Desktop/งาน กสศ. เต๊าะ/map/ข้อมูลพื้นฐานนักเรียน.xlsx")
#dmc<-dmc%>%dplyr::select(`ชื่อโรงเรียน`,
#                         `น้ำหนัก`,`ส่วนสูง`,
#                         `ความด้อยโอกาส`,
#                         `ขาดแคลนเครื่องแบบ`,
#                         `ขาดแคลนเครื่องเขียน`,
#                         `ขาดแคลนแบบเรียน`,
#                         `ขาดแคลนอาหารกลางวัน`,
#                         `ความพิการ`,
#                         `รายได้ต่อเดือนของบิดา`,
#                         `รายได้ต่อเดือนของมารดา`)
#names(dmc)<-c("school.name","weight","height","opp","uniform","stationary","book","lunch","disability","dad.income","mom.income")




amphor <- readOGR(dsn = "tha_admbnda_adm2_rtsd_20190221.shp", stringsAsFactors = F)
glimpse(amphor, max.level=2)
amphor@data%>%head()
map<-amphor[amphor$ADM2_TH=="แม่ฟ้าหลวง",]


sch.coord<-read.csv("school.coord.csv")
sch.coord2<-merge(sch.coord,dat2, by.x="sch.name",by.y="school.name")
#sch.coord2$direction<-c("right","right","left","bottom",
#                        "right","right","right","right",
#                        "left","bottom","right")

sch.location<-SpatialPointsDataFrame(coords=sch.coord2[,2:3], data=sch.coord2)



### ทำ cluster


problem.ind$id<-1:dim(problem.ind)[1]
problem.ind1<-problem.ind%>%
  mutate(sum.doctor=eye2+ear1+ear2+mouth2+skin2+digest1+digest2+digest3+
           nutrition2+nutrition3+move1+move2+move3)%>%
  dplyr::select(id,skin3, eye1, mouth1, hygiene1, 
                skin1, hygiene2, sum.doctor, nutrition1)

problem.ind1$prob.type<-NA

# คนที่ไม่ต้องรับของแน่ ๆ 
problem.ind2<-problem.ind1%>%mutate(sum=skin3+eye1+mouth1+hygiene1+skin1+hygiene2)%>%
  filter(sum==0)

# คนที่ต้องรับของ
problem.ind1<-problem.ind1%>%mutate(sum=skin3+eye1+mouth1+hygiene1+skin1+hygiene2)%>%
  filter(sum>0)

problem.ind1<-data.frame(problem.ind1)

set.seed(123)
clus42<-kmodes(problem.ind1[,c(2:7,11)], modes=42, iter.max=10)
problem.ind1$clus42<-clus42$cluster

type<-problem.ind1%>%group_by(clus42)%>%summarise(skin3 = mean(skin3),
                                            eye1 = mean(eye1),
                                            mouth1 = mean(mouth1),
                                            hygiene1 = mean(hygiene1),
                                            skin1 = mean(skin1),
                                            hygiene2= mean(hygiene2),
                                            sum.problem=mean(sum),
                                            n=n())%>%data.frame()%>%arrange(clus42,skin3,sum.problem)

#write.csv(type,"type.csv")

problem.ind2$clus42<-43
problem.student<-rbind(problem.ind1, problem.ind2)
#problem.student%>%head()
#write.csv(problem.student, "problem_cluster2.csv")

prob.solve<-read_excel("problemandsolve.xlsx")


temp.test<-merge(problem.student, prob.solve[,-1], by.x=c("skin3","eye1","mouth1","hygiene1","skin1","hygiene2"),
                 by.y=c("skin3","eye1","mouth1","hygiene1","skin1","hygiene2"))


temp.test$bmi<-ifelse(temp.test$nutrition1==1,1,0)

#problem.student<-merge(problem.student,prob.solve[,c(-2:-15)], by.x="clus42", by.y="clus42")
#problem.student$bmi<-ifelse(problem.student$nutrition1==1,1,0)

#write.csv(problem.student, file="problem_cluster.csv")


### studentdata

#studentdata<-read.csv("problem_cluster.csv")
studentdata<-temp.test
studentdata<-merge(problem.ind[,1:6],studentdata,by.x="id",by.y="id")

names(studentdata)[27:33]<-c("kill.louse","mouth.clean","skin.clean","odor","sanitary","glass","forward")

studentdata$room<-paste("ห้อง",studentdata$room)
studentdata$bmi<-ifelse(studentdata$bmi==1,"ต่ำกว่าเกณฑ์","ปกติ")
studentdata$forward<-ifelse(studentdata$mouth1==1 & studentdata$sum.doctor>0,
                            "ทันตแพทย์/แพทย์",
                            ifelse(studentdata$mouth1==1 & studentdata$sum.doctor==0,
                                   "ทันตแพทย์",
                                   ifelse(studentdata$mouth1==0 & studentdata$sum.doctor>0,
                                          "แพทย์","ไม่ต้อง")))
#     skin3, eye1, mouth1, hygiene1, skin1.x, 
#    hygiene3)
#doctor, sum.doc)




## choice ใน hover
problem<-c("การมองเห็น"="eye",
           "การได้ยิน"="ear",
           "ช่องปาก"="mouth",
           "ผิวหนัง"= "skin",
           "ทางเดินอาหาร" = "digest",
           "ทุพโภชนาการ" = "nutrition",
           "การเคลื่อนไหว" = "move",
           "สุขอนามัย" = "hygiene")





### ข้อมูล DMC

#dmc<-read_excel("ข้อมูลพื้นฐานนักเรียน.xlsx")
#head(dmc)
#glimpse(dmc)
#names(dmc)[c(1,2,4,5,7,9,10,14,17,18,19,20:24,25,63,64,79)]<-c("school.id","school.name",
#                                                               "level","room",
#                                                               "gender","student.name","student.surname",
#                                                               "age","nationality","race","religion",
#                                                               "old.bro","young.bro","old.sister","young.sister","order",
#                                                               "fam.status","weight","height","GPAX")
#dmc2<-dmc%>%select(1,2,4,5,7,9,10,14,17,18,19,20:24,25,63,64,79)

#dmc2%>%group_by(school.name)

ui <- bootstrapPage(
  

  navbarPage(theme = shinytheme("flatly"), collapsible = TRUE,
             HTML('<a style="text-decoration:none;cursor:default;color:#FFFFFF;" class="active" href="#">Health-Tracker</a>'), id="nav",
             windowTitle = "Health-Tracker",

             
             
             tabPanel("แผนที่สุขภาพนักเรียน",
                      div(class="outer",
                          tags$head(includeCSS("styles.css")), 
                          leafletOutput("map", width = "100%", height = "100%")),
                      
                      # absolutePanel(id="prob.type",top = 60, right = 10, 
                      #              selectInput(inputId="health", label="ปัญหาสุขภาพของนักเรียน",
                      #                         choices = problem)
                      #            )
                      #),
                      
                      absolutePanel(id = "controls", class = "panel panel-default",
                                    top = 200, left = 50, width = 570, fixed=TRUE,
                                    draggable = TRUE, height = "auto", 
                                    
                                    # span(tags$h5("ผลการสำรวจจากโครงการผลการจัดสรรความช่วยเหลือเพื่อลดปัญหาที่เป็นอุปสรรคต่อการเรียนรู้ของนักเรียน ในโรงเรียนนําร่องจังหวัดเชียงราย"), style="color:#045a8d"),
                                    h4("ผลสำรวจจากโครงการผลการจัดสรรความช่วยเหลือเพื่อลดปัญหาที่เป็นอุปสรรคต่อการเรียนรู้ของนักเรียน ในโรงเรียนนำร่องจังหวัดเชียงราย"),
                                    h5("กองทุนเพื่อความเสมอภาคทางการศึกษา ร่วมกับ คณะครุศาสตร์ จุฬาลงกรณ์มหาวิทยาลัย"),
                                    h5("ตุลาคม พ.ศ.2564"),
                                    h6("โรงเรียนที่เข้าร่วมโครงการ 11 โรงเรียน"),
                                    h6(sprintf("จำนวนนักเรียนทั้งหมด %g คน",
                                               comma(dim(dat)[1],digits=0))),
                                    h6(sprintf("นักเรียนที่มีปัญหาสุขภาพจำนวน %g คน",
                                               dim(problem.ind)[1], digits=0)),
                                    plotOutput("heatmap", height="500px"),
                                    
                                    selectInput(inputId="health", label="ปัญหาสุขภาพของนักเรียน",
                                                choices = problem),
                                    
                                    plotOutput("healthplot", height="130px", width="50%"),
                                    plotOutput("healthplot2", height="130px", width="50%")
                                    
                                    
                                    # h3(textOutput("reactive_case_count"), align = "right"),
                                    #h4(textOutput("reactive_death_count"), align = "right"),
                                    # h6(textOutput("clean_date_reactive"), align = "right"),
                                    #h6(textOutput("reactive_country_count"), align = "right"),
                                    #plotOutput("epi_curve", height="130px", width="100%"),
                                    #plotOutput("cumulative_plot", height="130px", width="100%"),
                                    
                                    # sliderTextInput("plot_date",
                                    #                label = h5("Select mapping date"),
                                    #               choices = format(unique(cv_cases$date), "%d %b %y"),
                                    #              selected = format(current_date, "%d %b %y"),
                                    #             grid = FALSE,
                                    #            animate=animationOptions(interval = 3000, loop = FALSE))
                                    
                      )
                      
             ),
             
        #     tabPanel("Summary",
                      
         #             dashboardPage(
          #              dashboardHeader(disable = TRUE),
           #             dashboardSidebar(
                         # selectInput(inputId="school", label="โรงเรียน",
                          #            choices = data.frame(dat2[,1])[,1]),
            #              selectInput(inputId="health2", label="ปัญหาสุขภาพของนักเรียน",
             #                         choices = problem)
              #          ),
               #         dashboardBody(
                #          fluidRow(
                 #           box(title="ปัญหาสุขภาพของนักเรียน",
                  #              plotlyOutput("school1"))
                   #       )
                    #    )
                     # )
                      
             #),
            tabPanel("ปัญหาสุขภาพของนักเรียนในโรงเรียน",
                     
                     fluidRow(
                       column(12,reactableOutput("school.table"))
                       )
                     
                     ),
             tabPanel("การช่วยเหลือนักเรียน",
                        fluidRow(
                        column(2,
                        selectInput(inputId="school", label="โรงเรียน",selected = "โรงเรียนบ้านเทอดไทย",
                                                    choices = names(table(studentdata[,2]))),
                        selectInput(inputId="level", label="ระดับชั้น",
                                    choices = names(table(problem.ind[,3]))),
                  #      checkboxGroupInput(inputId="list",label="ประเภทนักเรียน",
                   #                        c("ได้รับ Carebox" = ">0",
                    #                         "ควรส่งต่อเพื่อวินิจฉัย" = "1",
                     #                        "น้ำหนักส่วนสูงต่ำกว่าเกณฑ์"="1")),
                        # Button
                        downloadButton("downloadData", "Download")
                       # selectInput(inputId="room", label="ห้องเรียน",
                        #            choices = names(table(problem.ind[,4])))
                     #  checkboxGroupInput(inputId="room", label="ห้องเรียน",
                      #                     choices=names(table(problem.ind[,4])))
                                          # selected=names(table(dat3[,2])))),
                        
                       # column(1, ),
                        ),
                       column(7,
                       # box(title="จำนวนนักเรียนจำแนกตามระดับชั้น", plotOutput('sch.profile'), width=4),
                        box(plotlyOutput("school1"), width=6),
                        box(plotlyOutput("school2"), width=5)
                       )
                       # ),
                       ), #end of fluidRow
                        
                      fluidRow(
                        tabPanel("ฐานข้อมูลนักเรียนรายบุคคล",
                                 column(12, reactableOutput("student.table"))
                        ))
                       # tabPanel("About Us",
                        #         DT::dataTableOutput("table")
                        #)
                        
             )
  )
)

server <- function(input, output, session) {
  


  output$map <- renderLeaflet({
    
    sch<-sch.location[,c("lon","lat",input$health,"sch.name","total")]
    pal<-c("#FFB99A","#FF6464","#DB3056","#851D41")
    colorpal<-colorBin(palette=pal,sch@data[,3],3)
    
    m = leaflet(map) %>% addProviderTiles("OpenStreetMap",
                                          options = providerTileOptions(minZoom=11, maxZoom=14)) %>% 
      setView(sch.location$lon%>%mean()-0.04, sch.location$lat%>%mean(), zoom = 13)%>%
      addPolygons(col="grey",weight=2, smoothFactor=0.5, 
                  opacity=0.2, fillOpacity=0.2)%>%
      addCircles(data=sch,lng = ~lon, lat = ~lat, weight = 1,
                 radius = ~sqrt(sch@data[,3])*120, 
                 label = ~sch.name ,
                 col=~colorpal(sch@data[,3]), opacity=1.0,
                 labelOptions = labelOptions(noHide=T,textOnly = T,
                                             interactive=T, closeonClick   = T, permanent=TRUE))%>%
      addMarkers(data=sch.location,
                lng = ~lon, lat = ~lat,
                popup =  sprintf("<strong>%s</strong> <br/>
                                 จำนวนนักเรียนทั้งหมด %g คน <br/>
                                 มีปัญหา%sจำนวน %g คน",
                                         sch@data[,4],
                                         sch@data[,5],
                                         names(problem[problem==input$health]),
                                         sch@data[,3])%>%lapply(htmltools::HTML))
                      

      
    
    m %>% setMaxBounds(99.45562-0.04, 20.10693, 99.84080-0.04, 20.39433)
    
    
  })
  
  

  output$sch.profile<-renderPlot({
    dat4<-dat%>%filter(is.na(school.name)==FALSE, school.name==input$school)%>%
      group_by(level)%>%
      summarise(eye1=sum(eye1), eye2=sum(eye2),
                ear1=sum(ear1), ear2=sum(ear2),
                mouth1=sum(mouth1), mouth2=sum(mouth2),
                skin1=sum(skin1), skin2=sum(skin2), skin3=sum(skin3),
                digest1=sum(digest1), digest2=sum(digest2), digest3=sum(digest3),
                nutrition1=sum(nutrition1), nutrition2=sum(nutrition2), nutrition3=sum(nutrition3),
                move1=sum(move1), move2=sum(move2), move3=sum(move3),
                hygiene1=sum(hygiene1), hygiene2=sum(hygiene2),
                total=n())
   # dat4[8,1]<-"โรงเรียนบ้านปางมะหัน"
   #  dat4[9,1]<-"โรงเรียนบ้านพญาไพร"
    
   total.student<-data.frame(
     group=factor(dat4$level,levels=c(paste("อ.",1:3, sep=""), paste("ป.", 1:6,sep=""), paste("ม.",1:3,sep=""))),
     value=dat4$total
   )
   
   ggplot(total.student, aes(x=group, y=value))+geom_bar(stat="identity",fill="#0F2C67")+
     theme_minimal()+
     theme(text=element_text(family="ChulaCharasNew"))+scale_x_discrete(limits=rev)+
     xlab("ระดับชั้น \n")+ylab("\n จำนวนนักเรียน \n")+coord_flip()

    
  })
  
  
  output$heatmap<-renderPlot({
  
    
    ## ทำ heatmap
    
    problem2<-c("มองกระดานไม่ชัด"="eye1",
                "ตาแดง ตาอักเสบบ่อย"="eye2",
                "หูอื้อ/ฟังไม่ชัดเจน"="ear1",
                "หูอักเสบ/มีหนองไหลจากหู"="ear2",
                "ปวดฟันบ่อย/ฟันผุ" = "mouth1",
                "มีแผลในปาก/ร้อนใน" = "mouth2",
                "มีอาการคัน/มีตุ่ม ผื่น ตกสะเก็ดตามร่างกาย" = "skin1",
                "มีแผลเปื่อย หรือมีน้ำเหลือง/หนองตามร่างกาย" = "skin2",
                "มีเหา" = "skin3",
                "ถ่ายลำบาก/พบพยาธิตอนขับถ่าย" = "digest1",
                "ปวดท้อง/ท้องอืด/ท้องเฟ้อบ่อย" = "digest2",
                "ท้องเสียบ่อย" = "digest3",
                "น้ำหนัก/ส่วนสูงต่ำกว่าเกณฑ์" = "nutrition1",
                "ตัวซีด/ตัวเหลือง/ตาเหลือง" = "nutrition2",
                "ผิวหยาบ/ผมร่วง/ผมซีดกว่าปกติ" = "nutrition3",
                "ปวดหลัง/ปวดตัว/ปวดมือบ่อย" = "move1",
                "เดินกะเปลก" = "move2",
                "ใช้อุปกรณ์ช่วยในการเคลื่อนไหว" = "move3",
                "มีกลิ่นตัว/กลิ่นเท้า/กลิ่นปากแรง" = "hygiene1",
                "ประจำเดือนเปรอะเปื้อนตามเสื้อผ้า" = "hygiene2")
    
    x<-problem.ind
    
    x<-x%>%filter(is.na(school.name)==FALSE)%>%
      group_by(school.name)%>%
      summarise(eye1=sum(eye1),
                eye2=sum(eye2),
                ear1=sum(ear1),
                ear2=sum(ear2),
                mouth1=sum(mouth1),
                mouth2=sum(mouth2),
                skin1=sum(skin1),
                skin2=sum(skin2),
                skin3=sum(skin3),
                digest1=sum(digest1),
                digest2=sum(digest2),
                digest3=sum(digest3),
                nutrition1=sum(nutrition1),
                nutrition2=sum(nutrition2),
                nutrition3=sum(nutrition3),
                move1=sum(move1),
                move2=sum(move2),
                move3=sum(move3),
                hygiene1=sum(hygiene1),
                hygiene2=sum(hygiene2))%>%data.frame()
    
    colnames(x)<-c("level",names(problem2))
    x<-x%>%arrange(desc(`น้ำหนัก/ส่วนสูงต่ำกว่าเกณฑ์`))
    rownames(x)<-as.character(str_split(x[,1],"โรงเรียน", simplify = TRUE)[,2])
    
    library("gplots")
    library(RColorBrewer)
    par(family="ChulaCharasNew", mar=c(5,5,5,5))
    heatmap.2(as.matrix(t(x[,-1])), scale = "none", trace = "none", srtCol = 90,
              Rowv = T,
              density.info=c("histogram"),denscol="black", Colv=NA,
              cexCol=1.2, cexRow=1.2, adjCol=0.5, offsetCol=4,
              col=hcl.colors(8, palette = "reds",
                             alpha = NULL, 
                             rev = TRUE, 
                             fixup = TRUE),
              margins=c(6,14), 
              breaks=c(0,25,50,75,100,125,150,175,200), cellnote=as.matrix(t(x[,-1])),
              notecol="white",
              key.title = "จำนวนนักเรียนที่มีปัญหาสุขภาพ")

  })
  
### - pie chart
  
  output$healthplot<- renderPlot({
    sch<-sch.location[,c("lon","lat",input$health,"sch.name","total")]
    
    health<-data.frame(
      group<-c(input$health, "ไม่มีปัญหา"),
      value<-c(sum(sch@data[,3]), sum(sch@data[,5]-sum(sch@data[,3])))
    ) 
    
    ggplot(health, aes(x="", y=value, fill=group))+
      geom_bar(stat="identity", width=1, color="white")+
      coord_polar("y",start=0)+
      theme_void()+
      ggtitle(sprintf("มีปัญหาจำนวน %g คน", health$value[1])%>%lapply(htmltools::HTML))+
      theme(text=element_text(family='ChulaCharasNew', size=14))+
      labs(fill=sprintf("ปัญหา%s",names(problem[problem==input$health]))%>%lapply(htmltools::HTML))+
      scale_fill_discrete(labels=c("มีปัญหา","ไม่มีปัญหา"))
    
  })
  
### - barchart 
  
  output$healthplot2<-renderPlot({
    
    problem2<-c("มองกระดานไม่ชัด"="eye1",
               "ตาแดง ตาอักเสบบ่อย"="eye2",
               "หูอื้อ/ฟังไม่ชัดเจน"="ear1",
               "หูอักเสบ/มีหนองไหลจากหู"="ear2",
               "ปวดฟันบ่อย/ฟันผุ" = "mouth1",
               "มีแผลในปาก/ร้อนใน" = "mouth2",
               "มีอาการคัน/มีตุ่ม ผื่น ตกสะเก็ดตามร่างกาย" = "skin1",
               "มีแผลเปื่อย หรือมีน้ำเหลือง/หนองตามร่างกาย" = "skin2",
               "มีเหา" = "skin3",
               "ถ่ายลำบาก/พบพยาธิตอนขับถ่าย" = "digest1",
               "ปวดท้อง/ท้องอืด/ท้องเฟ้อบ่อย" = "digest2",
               "ท้องเสียบ่อย" = "digest3",
               "น้ำหนัก/ส่วนสูงต่ำกว่าเกณฑ์" = "nutrition1",
               "ตัวซีด/ตัวเหลือง/ตาเหลือง" = "nutrition2",
               "ผิวหยาบ/ผมร่วง/ผมซีดกว่าปกติ" = "nutrition3",
               "ปวดหลัง/ปวดตัว/ปวดมือบ่อย" = "move1",
               "เดินกะเปลก" = "move2",
               "ใช้อุปกรณ์ช่วยในการเคลื่อนไหว" = "move3",
               "มีกลิ่นตัว/กลิ่นเท้า/กลิ่นปากแรง" = "hygiene1",
               "ประจำเดือนเปรอะเปื้อนตามเสื้อผ้า" = "hygiene2")

    temp<-dat%>%dplyr::select(contains(input$health))
   # temp<-dat%>%dplyr::select(contains("eye"))
  #  temp<-ifelse(temp==1,0,1)
    dim<-dim(temp)[2]
    freq<-data.frame(group=colnames(temp)[1:dim],
                     value=colSums(temp[,1:dim]))
    ggplot(freq,aes(x=group,y=value))+geom_bar(stat="identity",fill="orange")+
      scale_x_discrete(labels=names(problem2[problem2%>%str_detect(input$health)]))+
      theme_bw()+xlab("")+ylab("จำนวนนักเรียน")+
      theme(text=element_text(family='ChulaCharasNew',size=16))+coord_flip()
      
    
  })
  
  
  output$school1<-renderPlotly({
    
    
    dat1.recoded<-problem.ind#%>%mutate_at(vars(7:40), ~ifelse(.==1,0,1))
    dat4<-dat1.recoded%>%filter(is.na(school.name)==FALSE)%>%
      group_by(school.name)%>%
      summarise(eye1=sum(eye1), eye2=sum(eye2),
                ear1=sum(ear1), ear2=sum(ear2),
                mouth1=sum(mouth1), mouth2=sum(mouth2),
                skin1=sum(skin1), skin2=sum(skin2), skin3=sum(skin3),
                digest1=sum(digest1), digest2=sum(digest2), digest3=sum(digest3),
                nutrition1=sum(nutrition1), nutrition2=sum(nutrition2), nutrition3=sum(nutrition3),
                move1=sum(move1), move2=sum(move2), move3=sum(move3),
                hygiene1=sum(hygiene1), hygiene2=sum(hygiene2),
                total=n())
   # dat4[7,1]<-"โรงเรียนบ้านปางมะหัน"
  #  dat4[8,1]<-"โรงเรียนบ้านพญาไพร"
    
    value<-dat4[dat4$school.name==input$school,-1]%>%data.frame()%>%as.numeric()

    t <- list(
      family = 'Noto Serif Thai')
    
    school<-str_split(input$school, "โรงเรียน", simplify = TRUE)[,2]
    
    fig <- plot_ly(
      
      labels = c(school, 
                 names(problem)[1], c("มองกระดานไม่ชัด","ตาแดง/อักเสบ"),
                 names(problem)[2], c("หูอื้อ","หูอักเสบ/มีหนอง"),
                 names(problem)[3], c("ปวดฟัน/ฟันผุ","แผลในปาก/ร้อนใน"),
                 names(problem)[4], c("คัน/มีตุ่ม/ผื่น","มีแผลเปื่อย/หนอง","มีเหา"),
                 names(problem)[5], c("ถ่ายลำบาก/พบพยาธิ","ปวดท้อง/ท้องอืด/ท้องเฟ้อ","ท้องเสียบ่อย"),
                 names(problem)[6], c("น้ำหนักส่วนสูงต่ำกว่าเกณฑ์","ตัวซีด/เหลือง/ตาเหลือง","ผิวหยาบ/ผมร่วง/ผมซีด"),
                 names(problem)[7], c("ปวดตัว/หลัง/มือ", "เดินกะเปลก","ใช้อุปกรณ์ช่วยในการเคลื่อนไหว"),
                 names(problem)[8], c("กลิ่นตัว/เท้า/ปาก แรง","ประจำเดือนเปรอะเปื้อน")
                 ), 
      
                                
      parents = c("", 
                  school, names(problem)[1], names(problem)[1],
                  school, names(problem)[2], names(problem)[2], 
                  school, names(problem)[3], names(problem)[3],
                  school, names(problem)[4], names(problem)[4], names(problem)[4],
                  school, names(problem)[5], names(problem)[5], names(problem)[5],
                  school, names(problem)[6], names(problem)[6], names(problem)[6],
                  school, names(problem)[7], names(problem)[7], names(problem)[7],
                  school, names(problem)[8], names(problem)[8]
                  ),
      
      values = c(value[21],
                 sum(value[1:2]), value[1:2],
                 sum(value[3:4]), value[3:4],
                 sum(value[5:6]), value[5:6],
                 sum(value[7:9]), value[7:9],
                 sum(value[10:12]), value[10:12],
                 sum(value[13:15]), value[13:15],
                 sum(value[16:18]), value[16:18],
                 sum(value[19:20]), value[19:20]),
      text = c("คน",rep("", 28)),
      type = 'sunburst',
      textinfo = "label+value+text"
    )
    
    fig%>%layout(font=t)
                # title = "ปัญหาสุขอนามัยของนักเรียนในโรงเรียน")

    
  })
  
  
  ### treemap
  
  output$school2<-renderPlotly({
    
    #studentdata<-read.csv("problem_cluster.csv")
    problem.ind.tree<-problem.ind%>%mutate(sum.doctor=eye2+ear1+ear2+mouth2+skin1+skin2+digest1+
                                        digest2+digest3+nutrition2+nutrition3+
                                        move1+move2+move3)
    
    studentdata.tree<-studentdata
      #merge(problem.ind.tree[,c(1:6,37,13)],studentdata%>%dplyr::select(-sum.doctor),by.x="id",by.y="id")
    #names(studentdata)[21:27]<-c("kill.louse","mouth.clean","skin.clean","odor","sanitary","glass","forward")
    
    #studentdata.tree$room<-paste("ห้อง",studentdata.tree$room)
    #studentdata.tree$bmi<-ifelse(studentdata.tree$bmi==1,"ต่ำกว่าเกณฑ์","ปกติ")
    #studentdata.tree$forward<-ifelse(studentdata.tree$mouth1+studentdata.tree$mouth2>0 & studentdata.tree$sum.doctor==1,
    #                            "ทันตแพทย์/แพทย์",
    #                            ifelse(studentdata.tree$mouth1+studentdata.tree$mouth2>0 & studentdata.tree$sum.doctor==0,
    #                                   "ทันตแพทย์",
    #                                   ifelse(studentdata.tree$mouth1+studentdata.tree$mouth2==0 & studentdata.tree$sum.doctor==1,
    #                                          "แพทย์","ไม่ต้อง")))
    
    schooldata_solve<-studentdata.tree%>%filter(is.na(school.name)==F)%>%group_by(school.name)%>%
                    summarise(kill.louse=sum(kill.louse),
                              mouth.clean=sum(mouth.clean),
                              skin.clean=sum(skin.clean),
                              odor=sum(odor),
                              sanitary=sum(sanitary),
                              glass=sum(glass))
    
    freq<-schooldata_solve%>%filter(school.name==input$school)

    fig<-plot_ly(
      type="treemap",
      labels=c("ชุดฆ่าเหา","ชุดทำความสะอาดช่องปาก","ชุดทำความสะอาดร่างกาย","ชุดกำจัดกลิ่นกาย" , "ผ้าอนามัย","วัดสายตา/ตัดแว่น"),
      parents=rep(input$school, 6),
      values=as.numeric(freq[1,2:7]),
      text=~paste(as.numeric(freq[1,2:7]),"หน่วย"),
      textinfo = "label+text",
      hoverinfo = "label+text"
    )
    
  })
  
  
  observe({
    sub<-problem.ind%>%filter(school.name==input$school)
    updateSelectInput(session, "level", choices = names(table(sub[,3])))
  })
  
  observe({
    sub2<-problem.ind%>%filter(school.name==input$school, level==input$level)
    updateCheckboxGroupInput(session, "room", choices= names(table(sub2[,4])))
  })
 
  
  output$student.table<-renderReactable({
    
   studentdata.reactab<-studentdata

#  ifelse(isTRUE(grepl("carebox",input$list)[1])==TRUE,
   #            <-c(names(studentdata)[26:31],input$list[-1]),var<-input$list)
   #เฉพาะที่มีปัญหาต้องแจกของ
    studentdata1<-studentdata.reactab%>%filter(school.name==input$school,
                                       level==input$level)%>%
      dplyr::select(room, name, surname,
                    names(studentdata)[26:34])
    
  
  theme <- reactableTheme(borderColor = "#dfe2e5", stripedColor = "#f6f8fa", 
                            highlightColor = "#f0f5f9", cellPadding = "8px 12px")
   
  reactable(studentdata1, 
            resizable = TRUE, 
            showPageSizeOptions = TRUE, 
            onClick = "expand", 
            highlight = TRUE, 
            
            borderless = TRUE,
            compact = TRUE, 
            fullWidth = TRUE,
            defaultColGroup = colGroup(headerClass = "group-header"),
            defaultColDef = colDef(class = "cell", headerClass = "header"),
            groupBy = "room",
    columnGroups = list(
    #  colGroup(name="", columns = "room"),
      #colGroup(name="ข้อมูลส่วนตัวนักเรียน", columns=c("name","surname")),
      colGroup(name="ของที่ได้รับ",
               columns=c("kill.louse","mouth.clean","skin.clean","odor","sanitary","glass"))),
    #  colGroup(name="ปัญหาสุขภาพทางกายที่พบในโรงเรียน", 
     #          columns = c("eye","ear","mouth","skin","digest","nutrition","move","hygiene"))
   # ),
    columns = list(
      room = colDef(name="ห้องเรียน",
                    defaultSortOrder = "desc",
                    minWidth = 100,
                    maxWidth = 110,
                    headerStyle = list(fontWeight = 700)
                   ),
      name = colDef(name="ชื่อ",
                           defaultSortOrder = "asc",
                           minWidth = 100,
                           maxWidth = 130,
                           headerStyle = list(fontWeight = 700),
                    details = function(index) {
                      tagList(
                        paste("ของนักเรียนที่ควรพบแพทย์:", index),
                        pre(paste(capture.output(studentdata1[index,]), collapse = " "))
                          )
                      }
                      ),  #end colDef of name
      surname = colDef(name="นามสกุล",
                         defaultSortOrder = "asc",
                         minWidth = 100,
                         maxWidth = 120,
                         headerStyle = list(fontWeight = 700)
                          ),
      details = colDef(name="ปัญหาทางสุขอนามัยที่พบ" ,
                         defaultSortOrder = "asc",
                         minWidth = 200,
                         maxWidth = 200,
                         headerStyle = list(fontWeight = 700),
                        footer="รวม"),
      kill.louse = knockout_column(name="ชุดฆ่าเหา",
                                   aggregate="sum", format = colFormat(suffix=" ชุด"),
                                   align="center",
                                   footer=function(values) sprintf("%.0f ชุด", sum(values))),
      mouth.clean = knockout_column(name="ชุดทำความสะอาดช่องปาก",
                                    aggregate="sum", format = colFormat(suffix=" ชุด"),
                                    align="center",
                                    footer=function(values) sprintf("%.0f ชุด", sum(values))),
      
      
      skin.clean =  knockout_column(name="ชุดทำความสะอาดร่างกาย",
                                    aggregate="sum", format = colFormat(suffix=" ชุด"),
                                    align="center",
                                    footer=function(values) sprintf("%.0f ชุด", sum(values))),
      
      
      odor = knockout_column(name="ชุดดับกลิ่นกาย",
                             aggregate="sum", format = colFormat(suffix=" ชุด"),
                             align="center",
                             footer=function(values) sprintf("%.0f ชุด", sum(values))),
      
      
      sanitary = knockout_column(name="ผ้าอนามัย",
                                 aggregate="sum", format = colFormat(suffix=" ชุด"),
                                 align="center",
                                 footer=function(values) sprintf("%.0f ชุด", sum(values))),
      
      
      glass = knockout_column(name="วัดสายตา/ตัดแว่น",
                              aggregate="sum", format = colFormat(suffix=" คน"),
                              align="center",
                              minWidth = 120,
                              maxWidth =120,
                              footer=function(values) sprintf("%.0f คน", sum(values))),
      
      
      
      bmi = colDef(name="น้ำหนัก/ส่วนสูง" ,
                   defaultSortOrder = "asc",
                   minWidth = 120,
                   maxWidth = 120,
                   headerStyle = list(fontWeight = 700),
                   align="left",
                   aggregate = "frequency"
      ),
      forward = colDef(name="การส่งต่อเพื่อวินิจฉัย" ,
                       defaultSortOrder = "asc",
                       minWidth = 120,
                       maxWidth = 150,
                       headerStyle = list(fontWeight = 700),
                       align="left",
                       aggregate = "frequency"
      )
      
      
    ),
   theme = theme)

  })
  
  
  
  #เฉพาะที่มีปัญหาต้องแจกของ
  
  
 
  # Reactive value for selected dataset ----
  thedataset <- reactive({
    
    #studentdata<-read.csv("problem_cluster.csv")
    problem.ind<-problem.ind%>%mutate(sum.doctor=eye2+ear1+ear2+mouth2+skin1+skin2+digest1+
                                        digest2+digest3+nutrition2+nutrition3+
                                        move1+move2+move3)
    
    studentdata.download<-merge(problem.ind[,c(1:6,13)],studentdata%>%dplyr::select(-sum.doctor),by.x="id",by.y="id")
    names(studentdata.download)[32:38]<-c("kill.louse","mouth.clean","skin.clean","odor","sanitary","glass","forward")
    
    studentdata.download$room<-paste("ห้อง",studentdata.download$room)
    #studentdata.download$bmi<-ifelse(studentdata.download$bmi==1,"ต่ำกว่าเกณฑ์","ปกติ")
   
    
    #เฉพาะที่มีปัญหาต้องแจกของ
   # studentdata1<-studentdata%>%filter(school.name==input$school,
    #                                   level==input$level)%>%
     # dplyr::select(room, name, surname,
      #              details, 21:28)
    
   # studentdata1
    
    studentdata1<-studentdata.download%>%filter(school.name.x==input$school)%>%
      dplyr::select(level.x, room.x, name.x, surname.x,
                    31:39)%>%arrange(level.x, room.x, name.x)
    names(studentdata1)<-c("ระดับชั้น","ห้องเรียน","ชื่อนักเรียน","นามสกุล","ปัญหาสุขอนามัยที่พบ",
                           "ชุดฆ่าเหา","ชุดทำความสะอาดช่องปาก","ชุดทำความสะอาดร่างกาย","ชุดดับกลิ่นกาย","ผ้าอนามัย","วัดสายตา/ตัดแว่น","การส่งต่อเพื่อวินิจฉัย/รักษา","น้ำหนักส่วนสูง")
    studentdata1
   # studentdata2<-studentdata%>%filter(school.name==input$school)%>%
    #                mutate(sum.doctor=ifelse(sum.doctor==1,"มี","ไม่พบ"))%>%
     #               dplyr::select(2,3,4,5,6,details,sum,21,22,23,24,25,26,bmi,7)
                    
      
    
   #names(studentdata2)<-c("โรงเรียน", "ระดับชั้น","ห้องเรียน","ชื่อ","นามสกุล","ปัญหาสุขอนามัยที่พบ","จำนวนความช่วยเหลือที่ได้รับ","ชุดฆ่าเหา","ชุดทำความสะอาดช่องปาก",
    #                       "ชุดทำความสะอาดร่างกาย","ชุดกำจัดกลิ่นตัว","ผ้าอนามัย", "วัดสายตา/ตัดแว่น", "น้ำหนักส่วนสูงต่ำกว่าเกณฑ์", "มีปัญหาอื่นที่ควรส่งต่อให้แพทย์วินิจฉัย")
  
   #studentdata2
    })
  
  
  output$downloadData <- downloadHandler(
    filename = function() {
      paste(input$school, ".xlsx", sep = "")
    },
    content = function(file) {
      write.xlsx(thedataset(), file, row.names=FALSE)
      #write.csv(thedataset(), file, row.names = FALSE) #fileEncoding="utf-8")
    }
  )
  
  
  
  
  output$school.table<-renderReactable({
    
   
    #schooldata2<-schooldata
    #schooldata2$school.name<-str_split(schooldata2$school.name, "โรงเรียน", simplify = TRUE)[,2]
    #schooldata2<-merge(schooldata2, dmc, by.x="school.name", by.y="school.name")
    #schooldata<-schooldata2
    reactable(
      schooldata,
      pagination = FALSE,
      defaultSorted = "mean.problem",
      defaultSortOrder = "desc",
      defaultColGroup = colGroup(headerClass = "group-header"),
      defaultColDef = colDef(class = "cell", headerClass = "header"),
      columnGroups = list(
      #  colGroup(name="รายได้ต่อเดือนของครอบครัว", columns=c("mean.income","min.income","max.income")),
        colGroup(name="จำนวนปัญหาต่อนักเรียน 1 คน", columns = c("num.problem","mean.problem","min.problem","max.problem")),
        colGroup(name="ปัญหาสุขภาพทางกายที่พบในโรงเรียน", 
                 columns = c("eye","ear","mouth","skin","digest","nutrition","move","hygiene"))
      ),
      columns = list(
        school.name = colDef(name="ชื่อโรงเรียน",
                             defaultSortOrder = "asc",
                             minWidth = 100,
                             maxWidth = 200,
                             headerStyle = list(fontWeight = 700)
        ),
        num.problem = colDef(name="Boxplot จำนวนปัญหาต่อคน",
                             minWidth = 100,
                             maxWidth = 120,
                             cell=function(values, index){
                               sparkline(schooldata$num.problem[[index]], type="box")
                             }),
        mean.problem = rating_column(name="Avg",format = colFormat(digits = 1), maxWidth = 60),
        min.problem = rating_column(name="Min", format = colFormat(digits = 1),  maxWidth = 60),
        max.problem = rating_column(name="Max", format = colFormat(digits = 1),  maxWidth = 60),
        
        eye = rating_column(name="ปัญหาทางตา", 
                            maxWidth =60, 
                            cell = function(value) {
                              scaled <- (value - min(schooldata$eye)) / (max(schooldata$eye) - min(schooldata$eye))
                              color <- off_rating_color(scaled)
                              value <- format(round(value, 0), nsmall = 0)
                              div(class = "spi-rating", style = list(background = color), value)
                            }),
        ear = rating_column(name="ปัญหาทางหู", 
                            maxWidth =60,
                            cell = function(value) {
                              scaled <- (value - min(schooldata$ear)) / (max(schooldata$ear) - min(schooldata$ear))
                              color <- off_rating_color(scaled)
                              value <- format(round(value, 0), nsmall = 0)
                              div(class = "spi-rating", style = list(background = color), value)
                            }),
        mouth = rating_column(name="ปัญหาช่องปาก", 
                              maxWidth =75,
                              cell = function(value) {
                                scaled <- (value - min(schooldata$mouth)) / (max(schooldata$mouth) - min(schooldata$mouth))
                                color <- off_rating_color(scaled)
                                value <- format(round(value, 0), nsmall = 0)
                                div(class = "spi-rating", style = list(background = color), value)
                              }),
        skin = rating_column(name="ปัญหาผิวหนัง", 
                             maxWidth =70,
                             cell = function(value) {
                               scaled <- (value - min(schooldata$skin)) / (max(schooldata$skin) - min(schooldata$skin))
                               color <- off_rating_color(scaled)
                               value <- format(round(value, 0), nsmall = 0)
                               div(class = "spi-rating", style = list(background = color), value)
                             }),
        digest = rating_column(name="ปัญหาทางเดินอาหาร", 
                               maxWidth =90,
                               cell = function(value) {
                                 scaled <- (value - min(schooldata$digest)) / (max(schooldata$digest) - min(schooldata$digest))
                                 color <- off_rating_color(scaled)
                                 value <- format(round(value, 0), nsmall = 0)
                                 div(class = "spi-rating", style = list(background = color), value)
                               }),
        nutrition = rating_column(name="ปัญหาทุพโภชนาการ", 
                                  maxWidth =90,
                                  cell = function(value) {
                                    scaled <- (value - min(schooldata$nutrition)) / (max(schooldata$nutrition) - min(schooldata$nutrition))
                                    color <- off_rating_color(scaled)
                                    value <- format(round(value, 0), nsmall = 0)
                                    div(class = "spi-rating", style = list(background = color), value)
                                  }),
        move = rating_column(name="ปัญหาการเคลื่อนไหว", 
                             maxWidth =90,
                             cell = function(value) {
                               scaled <- (value - min(schooldata$move)) / (max(schooldata$move) - min(schooldata$move))
                               color <- off_rating_color(scaled)
                               value <- format(round(value, 0), nsmall = 0)
                               div(class = "spi-rating", style = list(background = color), value)
                             }),
        hygiene = rating_column(name="ปัญหาสุขอนามัย", 
                                maxWidth =90,
                                cell = function(value) {
                                  scaled <- (value - min(schooldata$hygiene)) / (max(schooldata$hygiene) - min(schooldata$hygiene))
                                  color <- off_rating_color(scaled)
                                  value <- format(round(value, 0), nsmall = 0)
                                  div(class = "spi-rating", style = list(background = color), value)
                                })
        
        
      ))
    
     })
  
}


shinyApp(ui,server)
