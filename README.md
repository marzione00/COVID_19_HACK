# Covid_19 Rates Package

The package has a Shiny app to explore the Covid-19 outbreak in Italy.

## Lavora su
Andrea: <br>
Fabio: dir 3module <br>
Federico: <br>
Gregorio: <br>
Marzio: <br>


## Installation

```R
# First install the R package "devtools" if not installed
devtools::install_github('marzione00/COVID_19_HACK')
```

## Usage

Load the package

```R
require(Covid19)
```

The function of the package is `runC19()` and is run without arguments.

```R
runC19()
```

## Folders
```
covid19 
├── R
|   ├── get_countryTS.R
|   ├── get_provTS.R
|   ├── get_regionTS.R
|   └── runC19.R
|
├── inst
│   └── shiny
|         └── C19
│              ├── server
│              |     ├── 1module
|              |     |      └── map.R
│              |     ├── 2module
│              |     ├── 3module
│              |     ├── 4module
│              │     └── global
│              │            ├── shinyjs.R
│              │            └── sidebar.R
│              │   
│              ├── ui
│              |    ├── global
│              |    │     └── css.R
│              |    │       
│              |    └── tabs
│              |          ├── 1.tab_home.R
│              |          ├── 2.tab_inspection.R
│              |          ├── 3.tab_analysis.R
|              |          └── 4.tab_conclusion.R
|              ├── server.R
|              └── ui.R
├── man
│ 
├── .gitignore
├── .Rbuildignore
├── .Covid_19.Rproj
├── DESCRIPTION
├── NAMESPACE
└── README.md
```
