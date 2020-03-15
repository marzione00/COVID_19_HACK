<h1 align="center"> Covid_19 Rates Package </h1>

The package has a Shiny app to explore the Covid-19 outbreak in Italy.

## Lavora su
Andrea: implementing on master module3 - SIR on R - Editing Readme file <br>
Fabio: dir 3module <br>
Federico: <br>
Gregorio: <br>
Marzio: <br>


# Contributors :busts_in_silhouette:
- Andrea Ierardi   &nbsp; <a href="https://www.linkedin.com/in/andreaierardi/" rel="nofollow noreferrer">
    <img src="https://i.stack.imgur.com/gVE0j.png" alt="linkedin"> LinkedIn
  </a>  &nbsp;
  <a href="https://github.com/Andreaierardi" rel="nofollow noreferrer"> <img src="https://i.stack.imgur.com/tskMh.png" alt="github"> Github </a> &nbsp; 
  <a href="https://andreaierardi.github.io/" rel="nofollow noreferrer">  <img src="https://github.com/Andreaierardi/Master-DataScience-Notes/blob/master/img/website.svg" width=15>  Website </a>
- Marzio De Corato
- Gregorio Saporito
- Fabio Caironi
- Federico Matteucci

## Installation

```R
# First install the R package "devtools" if not installed
devtools::install_github('marzione00/COVID_19_HACK')
```

## Usage

Load the package

```R
require(covid19)
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
