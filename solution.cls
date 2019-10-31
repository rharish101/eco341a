% ======================================================================
% Assignment solution template by Harish Rajagopal
% ** Modified for ECO341A **
% ======================================================================

\NeedsTeXFormat{LaTeX2e}
\ProvidesClass{solution}[2018/08/25 Assignment solution class]
\LoadClass[11pt,a4paper]{report}

\DeclareOption*{\PassOptionsToClass{\CurrentOption}{article}}
\ProcessOptions\relax

\RequirePackage[margin=2.5cm]{geometry}  % For setting margins
\RequirePackage[gray]{xcolor}  % Make the document grayscale and define custom colors
\RequirePackage{ifthen}  % If-else in latex
\RequirePackage{minted}  % For code
\RequirePackage[fleqn]{amsmath}  % For the align environment used in custom math environments
\RequirePackage{environ}  % For custom math environments
\RequirePackage{mdframed}  % Boxes for custom math environments
\RequirePackage{tabularx}  % For custom auto-size columns in tables
\RequirePackage{booktabs}  % For customizing horizontal lines in tables
\RequirePackage{caption}  % For customizing caption spacing in tables
\RequirePackage{titlesec}  % For formatting sections, subsections, etc.
\RequirePackage{enumitem}  % Setting options for itemize and enumerate
\RequirePackage{hyperref}  % For coloring links and setting PDF info

\renewcommand{\familydefault}{\sfdefault}
\linespread{1.1}


% ======================================================================
% Code Blocks
% ======================================================================

% Background color for code blocks
\definecolor{codegray}{gray}{0.95}

% Code from a file
% Args:
%   #1: The programming language
%   #2: The input file path
\newcommand{\inputcode}[2]
{%
    \renewcommand{\theFancyVerbLine}
    {%
        \texttt{\textcolor[gray]{0.5}{\scriptsize{\arabic{FancyVerbLine}}}}
    }
    \vspace{-1mm}
    \inputminted%
    [
        bgcolor=codegray,
        baselinestretch=1.0,
        frame=single,
        framerule=0pt,
        framesep=0.5mm,
        linenos,
        numbersep=1mm,
        xleftmargin=9mm,
        autogobble,
        breaklines,
    ]%
    {#1}{#2}%
    \vspace{-8mm}%
}


% ======================================================================
% Math Box
% ======================================================================

% Allow equations across multiple pages
\allowdisplaybreaks%

% Formatting for math environments
\setlength{\mathindent}{3mm}
\setlength{\jot}{2mm}  % line spacing in amsmath

% With equation numbering
% Args:
%   #1: The underlying amsmath environment to use (eg. align, gather)
\RenewEnviron{math}[1][align]  % environ package used to avoid errors
{%
    \vspace{2mm}
    \begin{mdframed}%
    [
        linewidth=0.45pt,
        innerleftmargin=1mm,
        innertopmargin=-1mm,
        innerbottommargin=3mm,
    ]%
        \small
        \ifthenelse{\equal{#1}{align}}
            {\begin{align} \BODY \end{align}}
            {\begin{gather} \BODY \end{gather}}
    \end{mdframed}
    \vspace{1mm}
}

% Without equation numbering
% Args:
%   #1: The underlying amsmath environment to use (eg. align, gather)
\NewEnviron{math*}[1][align]  % environ package used to avoid errors
{%
    \vspace{2mm}
    \begin{mdframed}%
    [
        linewidth=0.45pt,
        innerleftmargin=1mm,
        innertopmargin=-1mm,
        innerbottommargin=3mm,
    ]%
        \small
        \ifthenelse{\equal{#1}{align}}
            {\begin{align*} \BODY \end{align*}}
            {\begin{gather*} \BODY \end{gather*}}
    \end{mdframed}
    \vspace{1mm}
}


% ======================================================================
% Tables
% ======================================================================

% Vertically align tabularx columns
\renewcommand{\tabularxcolumn}[1]{m{#1}}

% Columns for horizontal alignment
% NOTE: To be used only in tabularx
\newcolumntype{L}{>{\raggedright\arraybackslash}X}
\newcolumntype{C}{>{\centering\arraybackslash}X}
\newcolumntype{R}{>{\raggedleft\arraybackslash}X}

% Remove separation before and after horizontal lines in a table
\aboverulesep=0ex
\belowrulesep=0ex

% Increase cell height
\renewcommand{\arraystretch}{1.3}

% Increase spacing between table and its caption
\captionsetup[table]{skip=2mm}


% ======================================================================
% Formatting
% ======================================================================

% Chapter is used for formatting the solution environment
\titleformat{\chapter}{\bfseries\Large}{}{0mm}{}
\titlespacing{\chapter}{0mm}{1mm}{1mm}

% For sections, subsections and subsubsections
\titleformat{\section}{\large}{\thesection}{2mm}{\underline}
\titlespacing{\section}{0mm}{1mm}{1mm}
\titleformat{\subsection}{\itshape}{\thesubsection}{2mm}{}% chktex 6
\titlespacing{\subsection}{0mm}{0mm}{0mm}
\titleformat{\subsubsection}{}{\thesubsubsection}{2mm}{}

% Lengths
\setlength{\voffset}{-2mm}
\setlength{\footskip}{35pt}
\setlength{\parindent}{8mm}
\setlength{\parskip}{2mm}

% Used for "solution" environment and "\maketitle" command
\newcommand{\centerrule}{\begin{center}\rule{45mm}{0.5pt}\end{center}}


% ======================================================================
% Miscellaneous
% ======================================================================

% Lists
\setlist[itemize,1]{leftmargin=0mm,rightmargin=-0.5mm}
\setlist[enumerate,1]{leftmargin=0mm,itemsep=-1mm}

% Problem Solution Environment
% Args:
%   #1: The title for the question
\newenvironment{solution}[1][Question \thechapter]
{
    \ifthenelse{\equal{\thechapter}{0}}
        {\let\clearpage\relax}{}
    \chapter{#1:}
}
{\centerrule}


% ======================================================================
% Document Details
% ======================================================================

% Default author
\author{%
    Harish Rajagopal\\
    160552
}

% Set details
% Args:
%   #1: The title for the document
%   #2: The course number
%   #3: The course title
%   #4: The name of the assignment
\newcommand{\setdetails}[4][Solution]
{%
    \newcommand{\SolutionName}{#1}
    \newcommand{\CourseNo}{#2}
    \newcommand{\CourseName}{#3}
    \newcommand{\AssignName}{#4}
    \hypersetup%
    {%
        colorlinks,
        pdfinfo={%
            Author={Harish Rajagopal --- 160552},
            Title={{\CourseNo}: {\AssignName} {\SolutionName}},
            Subject={Solution of {\AssignName} for {\CourseNo}: {\CourseName}}
        }
    }
}

% My Modified Title
\renewcommand{\maketitle}
{%
    \begin{center}
        \rmfamily
        {\huge \SolutionName}\\[+2mm]
        {\Large \CourseNo: \CourseName\\[+2mm]
                \AssignName}\\[+3mm]
        \makeatletter
        {%
            \large
            \begin{tabular}[t]{c}
                \@author%
            \end{tabular}
        }
        \makeatother\\[+3mm]
    \end{center}
    \centerrule\vspace{1mm}
}
