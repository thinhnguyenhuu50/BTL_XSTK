\NeedsTeXFormat{LaTeX2e}
% \RequirePackage{expl3}

\ProvidesClass{hcmut_report}

\DeclareOption*{\PassOptionsToClass{\CurrentOption}{article}}
\ProcessOptions\relax
\LoadClass[a4paper]{article}

% Set up font
\RequirePackage[fontsize=12pt]{scrextend}
% \RequirePackage[utf8]{vntex}
% \RequirePackage[T5]{fontenc}
% \RequirePackage{mathptmx} % Apply Times New Roman font
\usepackage{fontspec}
\setmainfont{Times New Roman}
\RequirePackage{anyfontsize,amsmath,amssymb} % Fonts for math environment
\RequirePackage[vietnamese=nohyphenation]{hyphsubst} % Ignore a warning
\RequirePackage[vietnamese]{babel} % Support Vietnamese text

% Packages
\RequirePackage{fancyhdr,hyperref}
\RequirePackage[super,square,sort]{natbib}
\RequirePackage{tikz}
\usetikzlibrary{calc}
\RequirePackage{pgffor} % For loops
\RequirePackage{xstring} % For string manipulation
\RequirePackage{extramarks}
\RequirePackage[absolute,overlay]{textpos}
\setlength{\TPVertModule}{1cm}  % Set vertical measurement units to cm
\setlength{\TPHorizModule}{1cm}  % Set horizontal measurement units to cm
\RequirePackage{lastpage}

% Some packages that seem pretty redundant
\RequirePackage{geometry,graphicx,indentfirst,setspace}
\RequirePackage{booktabs}
\RequirePackage{caption,subcaption}

\RequirePackage{fancyhdr,hyperref}
\RequirePackage[super,square,sort]{natbib}

% Indentation
\setlength{\parindent}{20pt}

% Define some default names and a corresponding command to update them
\newcommand{\@upperuniname}{Vietnam National University Ho Chi Minh City}
\newcommand{\upperuniname}[1]{\renewcommand{\@upperuniname}{#1}}

\newcommand{\@uniname}{Ho Chi Minh City University of Technology}
\newcommand{\uniname}[1]{\renewcommand{\@uniname}{#1}}

\newcommand{\@deptname}{Faculty of Computer Science and Engineering}
\newcommand{\deptname}[1]{\renewcommand{\@deptname}{#1}}

\newcommand{\@coursename}{Course name}
\newcommand{\coursename}[1]{\renewcommand{\@coursename}{#1}}

\newcommand{\@reporttype}{Assignment Report}
\newcommand{\reporttype}[1]{\renewcommand{\@reporttype}{#1}}

\title{Report title}

% More arguments: top, bottom, left, right
\geometry{
  paper=a4paper,
  margin=2cm,
  includeheadfoot=true,
  headheight=40pt
}

\onehalfspacing
\raggedbottom
% Controls row height in tables
% \renewcommand{\arraystretch}{1.3}

% Set depth of numbering for sections and table of contents
\setcounter{secnumdepth}{4}
\setcounter{tocdepth}{3}

% Set depth of numbering for counters
\AtBeginDocument{\counterwithin{equation}{section}}
\AtBeginDocument{\counterwithin{table}{section}}
\AtBeginDocument{\counterwithin{figure}{section}}

\AtBeginDocument{\renewcommand*\bibname{References}}

\hypersetup{
  colorlinks=true,
  linkcolor=black,
  citecolor=red,
  urlcolor=blue
}

% Set up fancyhdr
% Line
\renewcommand{\headrulewidth}{0.3pt} 
\renewcommand{\footrulewidth}{0.3pt}
\linespread{1}
\pagestyle{fancy}
\fancyhf{}

\if@twoside
    \fancyhead[LO]{
        \bfseries
        \begin{tabular}{rl}
        \begin{picture}(25pt,15pt)
            \put(0,-8pt){\includegraphics[width=8mm, height=8mm]{graphics/hcmut.png}}
        \end{picture}&
        \begin{tabular}{l}
            \ttfamily TRƯỜNG ĐẠI HỌC BÁCH KHOA - ĐHQG HCM \\
            \ttfamily \@deptname \\
        \end{tabular}
        \end{tabular}
    }
    \fancyhead[RE]{
        \bfseries
        \begin{tabular}{rl}
        \begin{tabular}{r}
            \ttfamily TRƯỜNG ĐẠI HỌC BÁCH KHOA - ĐHQG HCM \\
            \ttfamily \@deptname \\
        \end{tabular}
        \begin{picture}(25pt,15pt)
            \put(0,-8pt){\includegraphics[width=8mm, height=8mm]{graphics/hcmut.png}}
        \end{picture}
        \end{tabular}
    }
\else
    \fancyhead[R]{
        \bfseries
        \begin{tabular}{rl}
        \begin{tabular}{r}
            \ttfamily\@uniname \\
            \ttfamily\@deptname \\
        \end{tabular}
        \begin{picture}(25pt,15pt)
            \put(0,-8pt){\includegraphics[width=20pt]{graphics/hcmut.png}}
        \end{picture}
        \end{tabular}
    }
\fi

\fancyfoot[LO]{\scriptsize \ttfamily Trang \thepage/\pageref{LastPage}}
\fancyfoot[RO]{\scriptsize \ttfamily Báo cáo Bài tập lớn Xác suất và thống kê - HK241}
\fancyfoot[LE]{\scriptsize \ttfamily Báo cáo Bài tập lớn Xác suất và thống kê - HK241}
\fancyfoot[RE]{\scriptsize \ttfamily Trang \thepage/\pageref{LastPage}}
%------------------------------------------------------------------------------------------------------------------------%
\newcommand{\coverpage}{
    
    \begin{tikzpicture}[overlay,remember picture]
        \draw [line width=3pt]        
            ($ (current page.north west) + (2.5cm,-1.5cm) $)
            rectangle
            ($ (current page.south east) + (-1.5cm,1.5cm) $);        
        \draw [line width=0.5pt]
            ($ (current page.north west) + (2.6cm,-1.6cm) $)
            rectangle
            ($ (current page.south east) + (-1.6cm,1.6cm) $);
    \end{tikzpicture}

    \pagestyle{empty}

    \vspace*{-3\baselineskip}
    \begin{center}
        \MakeUppercase{\@upperuniname}

        \MakeUppercase{\@uniname}

        \MakeUppercase{\@deptname}
        \vfill
        \includegraphics[scale=0.4]{graphics/hcmut.png}
        \vfill
        \bfseries
        \begin{tabular}{c}
        \large\@coursename                                 \\
        % {}                                                 \\
        \midrule                                           \\
        % {}                                                 \\
        \Large\@reporttype                               \\
        {}                                                 \\
        \Huge\parbox[c]{0.8\linewidth}{\centering \@title} \\
        % {}                                                 \\
        \bottomrule
        \end{tabular}
        \vfill
        \large LỚP TN01 -- NHÓM 10 -- HK241
        \vfill
        \mdseries
        \begin{tabular}{llr}
            Giảng viên hướng dẫn: \@advisor  
            Sinh viên thực hiện: \@student  
        \end{tabular}
        \vfill
        \begin{textblock}{10}[0.5,0.5](10.5,27)
            \MakeUppercase{Thành phố Hồ Chí Minh, tháng 11/2024}
        \end{textblock}
    \end{center}
}

\ExplSyntaxOn 
\NewDocumentCommand{\advisor}{+v}{
    \seq_set_split:Nnn \l_tmpa_seq { \obeyedline } {#1}    
    \seq_remove_all:Nn \l_tmpa_seq {}
    
    \tl_const:Ne \l_advisor_name_tl {& \seq_use:Nn \l_tmpa_seq {} & \\}   

    \newcommand{\@advisor}{\tl_use:N \l_advisor_name_tl}    
}

\NewDocumentCommand{\student}{+v}{
    \seq_set_split:Nnn \l_tmpa_seq { \obeyedline } {#1}
    \seq_remove_all:Nn \l_tmpa_seq {}

    \tl_clear_new:N \l_student_list_tl    

    \seq_map_inline:Nn \l_tmpa_seq {
        \seq_set_split:Nnn \l_tmpb_seq { , } {##1}        
        \tl_put_right:Ne \l_student_list_tl { & \seq_use:Nn \l_tmpb_seq { & } \\}        
    }    
    \newcommand{\@student}{\tl_use:N \l_student_list_tl}
}
\ExplSyntaxOff