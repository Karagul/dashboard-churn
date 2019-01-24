#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# DASHBOARD PELANGGAN CHURN
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Aplikasi ini mencakup bagaimana cara membuat: 
# - ringkasan dari sumber data (manipulasi data)
#   menggunakan dplyr
# - modifikasi tema ggplot2
# - grafik interaktif
# - grafik biasa
# - dashboard berbasis web yang dapat diakses di
#   shinyapps.io
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# INISIASI PAKET YANG DIBUTUHKAN
# - tidyverse untuk manipulasi data
# - shinydashboard untuk membuat dashboard
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

library(tidyverse)
library(shinydashboard)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# BUKA DATA
# Baca file csv dan simpan di variabel "data".
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

data <- read.csv("WA_Fn-UseC_-Telco-Customer-Churn.csv")

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# MANIPULASI DATA
# Membuat data tabel baru untuk menganalisis
# faktor yang paling tinggi mempengaruhi Churn
# pelanggan menggunakan paket dari dplyr.
# Variabel masing-masing field dihitung jumlah 
# pelanggannya.
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

ringkasan <- data %>% 
  group_by(Churn) %>% 
  summarise(
    Pria = length(customerID[gender == "Male"]),
    Wanita = length(customerID[gender == "Female"]),
    Senior = length(customerID[SeniorCitizen == 1]),
    Non.Senior = length(customerID[SeniorCitizen == 0]),
    Pasangan = length(customerID[Partner == "Yes"]),
    Non.Pasangan = length(customerID[Partner == "No"]),
    Mandiri = length(customerID[Dependents == "Yes"]),
    Non.Mandiri = length(customerID[Dependents == "No"]),
    Telepon = length(customerID[PhoneService == "Yes"]),
    Non.Telepon = length(customerID[PhoneService == "No"]),
    Multi.Line = length(customerID[MultipleLines == "Yes"]),
    Non.Multi.Line = length(customerID[MultipleLines == "No"]),
    Layanan.DSL = length(customerID[InternetService == "DSL"]),
    Layanan.FO = length(customerID[InternetService == "Fiber optic"]),
    Non.Layanan = length(customerID[InternetService == "No"]),
    OL.Security = length(customerID[OnlineSecurity =="Yes"]),
    Non.OL.Security = length(customerID[OnlineSecurity =="No"]),
    OL.Backup = length(customerID[OnlineBackup =="Yes"]),
    Non.OL.Backup = length(customerID[OnlineBackup =="No"]),
    Proteksi = length(customerID[DeviceProtection =="Yes"]),
    Non.Proteksi= length(customerID[DeviceProtection =="No"]),
    Support = length(customerID[TechSupport =="Yes"]),
    Non.Support = length(customerID[TechSupport =="No"]),
    StreamTV = length(customerID[StreamingTV =="Yes"]),
    Non.StreamTV = length(customerID[StreamingTV =="No"]),
    StreamMov = length(customerID[StreamingMovies =="Yes"]),
    Non.StreamMov = length(customerID[StreamingMovies =="No"]),
    Kontrak.Bulanan = length(customerID[Contract == "Month-to-month"]),
    Kontrak.Setahun = length(customerID[Contract == "One year"]),
    Kontrak.Duatahun = length(customerID[Contract == "Two year"]),
    Tagihan.Kertas = length(customerID[PaperlessBilling == "No"]),
    Non.Tagihan.Kertas = length(customerID[PaperlessBilling == "Yes"]),
    Bayar.Bank = length(customerID[PaymentMethod == "Bank transfer (automatic)"]),
    Bayar.Kredit = length(customerID[PaymentMethod == "Credit card (automatic)"]),
    Bayar.Cek = length(customerID[PaymentMethod == "Electronic check"]),
    Bayar.Cek.Email = length(customerID[PaymentMethod == "Mailed check"])
  )

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# MENGUBAH DATA WIDE FORMAT KE LONG FORMAT
# Diubah ke long format untuk mempermudah
# perhitungan dan meringkas data.
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
ringkasan2 <- gather(ringkasan, "Faktor", "Jml.Pelanggan", 2:37)


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# MERINGKAS DATA
# Menghitung persentase churn masing-masing 
# faktor 
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

ringkasan.churn <- ringkasan2 %>% 
  group_by(Faktor) %>% 
  summarise(pelanggan = sum(Jml.Pelanggan), 
            churn = sum(Jml.Pelanggan[Churn == "Yes"]) / sum(Jml.Pelanggan) * 100) %>% 
  arrange(desc(churn))

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# MODIFIKASI TEMA
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

tema <-
  theme(text = element_text(family = "Verdana", color = "#444444")) +
  theme(plot.title = element_text(size = 24)) +
  theme(plot.subtitle = element_text(size = 18)) +
  theme(axis.title = element_text(size = 14)) +
  theme(axis.title.y = element_text(angle = 0, vjust = .5, margin = margin(r = 15))) +
  theme(axis.text = element_text(size = 10)) +
  theme(axis.title.x = element_text(margin = margin(t = 20))) +
  theme(legend.title = element_blank())

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# JUDUL DASHBOARD
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

header <- dashboardHeader(
    title = HTML(paste(icon("cubes"), "Dashboard"))
  )

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# MENU DASHBOARD
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

sidebar <- dashboardSidebar(
  sidebarMenu(id = "tabs", 
              menuItem("Pelanggan",
                       tabName = "pelanggan", 
                       icon = icon("address-book")
              )
  )
)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# KOTAK INFORMASI TOTAL PELANGGAN
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
info.1 <- valueBox(
  value = length(data$customerID),
  subtitle = "total pelanggan", 
  icon = icon("users"), width = 3
)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# KOTAK INFORMASI TOTAL PENDAPATAN
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

info.2 <- valueBox(
  value = formatC(sum(data$TotalCharges, na.rm = "yes")/1000000, 
                  big.mark = ".", decimal.mark = ",", 
                  format = "f", digits = 2),
  subtitle = "total pendapatan ($juta)", 
  icon = icon("money"), width = 3
)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# KOTAK INFORMASI TOTAL PELANGGAN CHURN
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

info.3 <- valueBox(
  value = length(data$customerID[data$Churn == "Yes"]),
  subtitle = "pelanggan churn",
  icon = icon("user-times"), width = 3
)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# KOTAK INFORMASI PERSENTASE CHURN PELANGGAN
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

info.4 <- valueBox(
  value = formatC(length(data$customerID[data$Churn == "Yes"]) /
                    length(data$customerID) * 100, big.mark = ".", 
                  decimal.mark = ",", format = "f", digits = 2),
  subtitle = "persen churn (%)",
  icon = icon("line-chart"), width = 3
)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# GRAFIK FAKTOR LAYANAN INTERNET
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

box.grafik.1 <- box(
  title = "Faktor Layanan Internet",
  solidHeader = TRUE,
  radioButtons(
    inputId = "keterangan1",
    label = "Churn?",
    choices = c("Ya" = "Yes", "Tidak" = "No"),
    inline = TRUE
  ),
  plotOutput("grafik1", height = "150px"),
  width = 6,
  status = "primary"
)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# GRAFIK FAKTOR KONTRAK LANGGANAN
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

box.grafik.2 <- box(
  title = "Faktor Kontrak Langganan",
  solidHeader = TRUE,
  radioButtons(
    inputId = "keterangan2",
    label = "Churn?",
    choices = c("Ya" = "Yes", "Tidak" = "No"),
    inline = TRUE
  ),
  plotOutput("grafik2", height = "150px"),
  width = 6,
  status = "primary"
)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# KOTAK INFROMASI ANALISIS CHURN
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

box.analisis <- box(
  title = "Analisis Churn",
  solidHeader = TRUE,
  plotOutput("grafik.analisis", height = "750px"),
  width = 12,
  status = "primary"
)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# MENGGABUNGKAN SEMUA GRAFIK DI BODY DASHBOARD
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

body <- dashboardBody(
  tabItems(
    tabItem(tabName = "pelanggan",
            fluidRow(info.1, info.2, info.3, info.4, 
                     box.grafik.1, box.grafik.2, box.analisis))
  )
)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# MENGGABUNGKAN SEMUA ELEMEN VISUAL DASHBOARD
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

ui <- dashboardPage(header, sidebar, body)
  
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# MEMETAKAN FUNGSI INPUT DAN OUTPUT ELEMEN
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

server <- function(input, output) {

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# GRAFIK INTERAKTIF LAYANAN INTERNET
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

output$grafik1 <- renderPlot({
    data %>%
      group_by(InternetService) %>%
      filter(Churn == input$keterangan1) %>%
      summarise(Jumlah.Pelanggan = n()) %>%
      ggplot(aes(y = Jumlah.Pelanggan, x = InternetService)) +
      geom_bar(stat = "identity", fill = "#0072B2") +
      geom_text(aes(label = Jumlah.Pelanggan), hjust = 1.1, color = "white") +
      coord_flip() + 
      labs(y = "Jumlah Pelanggan", x = NULL) +
      tema
  })

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# GRAFIK INTERAKTIF FAKTOR KONTRAK LANGGANAN
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

output$grafik2 <- renderPlot({
  data %>%
    group_by(Contract) %>%
    filter(Churn == input$keterangan2) %>%
    summarise(Jumlah.Pelanggan = n()) %>%
    ggplot(aes(y = Jumlah.Pelanggan, x = Contract)) +
    geom_bar(stat = "identity", fill = "#0072B2") +
    geom_text(aes(label = Jumlah.Pelanggan), hjust = 1.1, color = "white") +
    coord_flip() + 
    labs(y = "Jumlah Pelanggan", x = NULL) +
    tema
})

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# GRAFIK ANALISIS FAKTOR CHURN
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

output$grafik.analisis <- renderPlot({
  ringkasan.churn %>% 
    ggplot(aes(y = churn, x = reorder(Faktor, churn))) + 
    geom_bar(stat = "identity", fill = "#0072B2") + 
    geom_text(aes(label = round(churn, 1)), hjust = 1.1, color = "white") + 
    coord_flip() + ggtitle("Faktor Churn Pelanggan") + 
    labs(y = "Persen Churn", x = NULL) + 
    tema
  
})

}

shinyApp(ui = ui, server = server)