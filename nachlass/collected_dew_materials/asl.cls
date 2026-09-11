% asl.cls, Version 1.2, September 14, 2000 
% Yiannis N. Moschovakis, ynm@math.ucla.edu
% Copyright 2000 by the Association for Symbolic Logic
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% This file carries the usual TEX-file non-warranty for results
% and it can be freely copied and re-distributed provided that: 
%
% IF IT IS CHANGED IN THE SLIGHTEST WAY, it must be RENAMED 
%
% With code cribbed from the 1996/11/05 Version 1.2q of amsart.cls
% and ancillary AMSLATEX packages, in 1996 versions. 
% An attempt to load one of these packages gives an error message.
% For a list of the relevant packages and versions
% search for \errmessage below or see asldoc.
% 
% The ASL-specific code has been adapted to latex2e by YNM from an
% early version of AMSLATEX for the ASL prepared by the AMS
% programming staff. This version includes the code from all
% necessary stylefiles except for amsfonts, amssymb and latexsym
% 
% It is assumed that all LATEX2e files exist
% in versions not earlier than 1994/12/01
% including latexsym.sty, which is automatically loaded.
%
% The packages amsfonts.sty and amssymb.sty are loaded if they 
% exist, otherwise a warning is given that ams fonts are not 
% available and compilation does not stop until some amsfont 
% symbol is required.  
%
%%%%%%%%%%%%%%%% Changes 
% bibextra.sty minor (diagnostic) bug corrected, 4/6/00
% Version 1.1, August 20, 2000
% Introduced option bibother which works with 
% non-asl bibliography styles and hand-set bibliographies
%
% Added an optional argument to \subjclass
% which distinguishes between 1991 and 2000 classifications
% to make it friendlier to new amslatex users
%
% 9/14/2000 Corrected numbering error in eqnarray environment
%%%%%%%%%%%%%%%%%

\NeedsTeXFormat{LaTeX2e}% LaTeX 2.09 can't be used (nor non-LaTeX)
[1994/12/01]% LaTeX date must December 1994 or later
\ProvidesClass{asl}[2000/09/14 v1.2]
\def\@tempa#1#2\@nil{\edef\@classname{#1}}
\expandafter\@tempa\@currnamestack{}{}{}\@nil
\ifx\@classname\@empty \edef\@classname{\@currname}\fi

%%%%%%%%%%%%%%% Give error if called to load a known amslatex pkg
\def\aslknown{0}
\let\oldusepackage=\usepackage
\renewcommand{\usepackage}[2][]{%
\if\csname#2case\endcsname\aslknown\csname#2aslerr\endcsname
\else \oldusepackage[#1]{#2}\fi}

\def\amsmathcase{0}
\def\amsmathaslerr{
\errmessage{asl.cls already knows amsmath 
version 1.2c [1996/11/01]^^J
and cannot load amsmath.^^J
Hit RETURN to continue!^^J
}}

\def\amsgencase{0}
\def\amsgenaslerr{
\errmessage{^^J asl.cls already knows amsgen 
version 1.2b [1996/10/29]^^J
and cannot load amsgen.^^J
Hit RETURN to continue!^^J
}}

\def\amstextcase{0}
\def\amstextaslerr{
\errmessage{^^J asl.cls already knows amstext 
version 1.2b [1996/10/28]^^J
and cannot load amstext.^^J
Hit RETURN to continue!^^J
}}

\def\amsbsycase{0}
\def\amsbsyaslerr{
\errmessage{^^J asl.cls already knows amsbsy 
version 1.2b [1996/10/29]^^J
and cannot load amsbsy.^^J
Hit RETURN to continue!^^J
}}

\def\amsopncase{0}
\def\amsopnaslerr{
\errmessage{^^J asl.cls already knows amsopn 
version 1.2b [1996/10/28]^^J
and cannot load amsopn.^^J
Hit RETURN to continue!^^J
}}

%%%%%%%%%%%%%%%%%%%%%%% Options from amsart
\DeclareOption{a4paper}{\paperheight 297mm\paperwidth 210mm
  \textheight 54.5pc }
\DeclareOption{letterpaper}{\paperheight 11in\paperwidth 8.5in }
\DeclareOption{landscape}{\@tempdima\paperheight
  \paperheight\paperwidth \paperwidth\@tempdima}
\DeclareOption{portrait}{}
\DeclareOption{oneside}{\@twosidefalse \@mparswitchfalse}
\DeclareOption{twoside}{\@twosidetrue  \@mparswitchtrue}
\DeclareOption{draft}{\overfullrule5\p@ }
\DeclareOption{final}{\overfullrule\z@ }
\def\dateposted#1{\def\@dateposted{\ (#1)}}%
\let\@dateposted\@empty
\DeclareOption{e-only}{%
  \def\volinfo{Volume \currentvolume}%
  \dateposted{Xxxx XX, XXXX}%
}
\newif\if@titlepage
\DeclareOption{titlepage}{\@titlepagetrue}
\DeclareOption{notitlepage}{\@titlepagefalse}
\newif\if@openright
\DeclareOption{openright}{\@openrighttrue}
\DeclareOption{openany}{\@openrightfalse}
\DeclareOption{onecolumn}{\@twocolumnfalse}
\DeclareOption{twocolumn}{\@twocolumntrue}
\DeclareOption{nomath}{}
\def\mathfrak{needed}
\DeclareOption{noamsfonts}{\let\mathfrak\relax}
\DeclareOption{psamsfonts}{%
  \PassOptionsToPackage{psamsfonts}{amsfonts}%
%  \PassOptionsToPackage{cmex10}{amsmath}}
   \ExecuteOptions{cmex10}}     % this option is declared here 
% Commands preparatory to declaring pointsize options
\newcommand{\@mainsize}{10}
\newcommand{\@ptsize}{0}
\newcommand{\larger}[1][1]{%
  \count@\@currsizeindex \advance\count@#1\relax
  \ifnum\count@<\z@ \count@\z@ \else\ifnum\count@>12 \count@12 \fi\fi
  \ifcase\count@
    \Tiny\or\Tiny\or\tiny\or\SMALL\or\Small\or\small
    \or\normalsize
    \or\large\or\Large\or\LARGE\or\huge\or\Huge\else\Huge
  \fi
}
\newcommand{\smaller}[1][1]{\larger[-#1]}
\def\@adjustvertspacing{%
  \bigskipamount.7\baselineskip plus.7\baselineskip
  \medskipamount\bigskipamount \divide\medskipamount\tw@
  \smallskipamount\medskipamount \divide\smallskipamount\tw@
  \abovedisplayskip\medskipamount
  \belowdisplayskip \abovedisplayskip
  \abovedisplayshortskip\abovedisplayskip
  \advance\abovedisplayshortskip-1\abovedisplayskip
  \belowdisplayshortskip\abovedisplayshortskip
  \advance\belowdisplayshortskip 1\smallskipamount
  \jot\baselineskip \divide\jot 4 \relax
}
\renewcommand\normalsize{\@xsetfontsize\normalsize 6%
  \@adjustvertspacing \let\@listi\@listI}
\newcommand\Tiny{\@xsetfontsize\Tiny 1}
\newcommand\tiny{\@xsetfontsize\tiny 2}
\newcommand\SMALL{\@xsetfontsize\SMALL 3}
\newcommand\Small{\@xsetfontsize\Small 4%
  \@adjustvertspacing
  \def\@listi{\topsep\smallskipamount \parsep\z@skip \itemsep\z@skip}}
\newcommand\small{\@xsetfontsize\small 5\@adjustvertspacing}
\def\footnotesize{\Small}
\def\scriptsize{\SMALL}
\newcommand\large{\@xsetfontsize\large 7\@adjustvertspacing}
\newcommand\Large{\@xsetfontsize\Large 8\@adjustvertspacing}
\newcommand\LARGE{\@xsetfontsize\LARGE 9}
\newcommand\huge{\@xsetfontsize\huge{10}}
\newcommand\Huge{\@xsetfontsize\Huge{11}}
\def\@xsetfontsize#1#2{%
  \chardef\@currsizeindex#2\relax
  \edef\@tempa{\@nx\@setfontsize\@nx#1%
    \@xp\ifcase\@xp\@currsizeindex\@typesizes
      \else{99}{99}\fi}%
  \@tempa
}
\chardef\@currsizeindex=6
\widowpenalty=10000
\clubpenalty=10000
\brokenpenalty=10000
\newdimen\linespacing
\lineskip=1pt \lineskiplimit=1pt
\normallineskip=1pt \normallineskiplimit=1pt
\let\baselinestretch=\@empty
\headheight=5pt \headsep=14pt                  % redefined in asl
\footskip=12pt                                 % redefined in asl
\textheight=50.5pc \topskip=10pt               % redefined in asl
\textwidth=30pc                                % redefined in asl
\columnsep=10pt \columnseprule=0pt
\marginparwidth=90pt
\marginparsep=11pt
\marginparpush=5pt
\AtBeginDocument{\settoheight{\footnotesep}{\footnotesize M$^1$}}
\skip\footins=7pt plus11pt
\skip\@mpfootins=\skip\footins
\fboxsep=3pt \fboxrule=.4pt
\arrayrulewidth=.4pt    \doublerulesep=2pt
\labelsep=5pt           \arraycolsep=\labelsep
\tabcolsep=\labelsep    \tabbingsep=\labelsep
\floatsep=15pt plus 12pt                     % redefined in asl
\dblfloatsep=15pt plus 12pt
\textfloatsep=\floatsep                      % redefined in asl
\dbltextfloatsep=15pt plus 12pt
\intextsep=\floatsep
\@fptop=0pt plus1fil     \@dblfptop=0pt plus1fil
\@fpbot=0pt plus1fil     \@dblfpbot=0pt plus1fil
\@fpsep=8pt plus2fil     \@dblfpsep=8pt plus2fil\relax
\parskip=0pt \relax
\newdimen\normalparindent
\normalparindent=12pt                        % redefined in asl
\parindent=\normalparindent
\partopsep=0pt \relax \parsep=0pt \relax \itemsep=0pt \relax
\@lowpenalty=51         \@medpenalty=151        \@highpenalty=301
\@beginparpenalty=-\@lowpenalty
\@endparpenalty=-\@lowpenalty
\@itempenalty=-\@lowpenalty
\DeclareOption{10pt}{\def\@mainsize{10}\def\@ptsize{0}%
  \def\@typesizes{%
    \or{5}{6}\or{6}{7}\or{7}{8}\or{8}{10}\or{9}{11}%
    \or{10}{12}% normalsize
    \or{\@xipt}{13}\or{\@xiipt}{14}\or{\@xivpt}{17}%
    \or{\@xviipt}{20}\or{\@xxpt}{24}}%
  \normalsize \linespacing=\baselineskip
}
\DeclareOption{11pt}{\def\@mainsize{11}\def\@ptsize{1}%
  \def\@typesizes{%
    \or{6}{7}\or{7}{8}\or{8}{10}\or{9}{11}\or{10}{12}%
    \or{\@xipt}{13}% normalsize
    \or{\@xiipt}{14}\or{\@xivpt}{17}\or{\@xviipt}{20}%
    \or{\@xxpt}{24}\or{\@xxvpt}{30}}%
  \normalsize \linespacing=\baselineskip
}
\DeclareOption{12pt}{\def\@mainsize{12}\def\@ptsize{2}%
  \def\@typesizes{%
    \or{7}{8}\or{8}{10}\or{9}{11}\or{10}{12}\or{\@xipt}{13}%
    \or{\@xiipt}{14}% normalsize
    \or{\@xivpt}{17}\or{\@xviipt}{20}\or{\@xxpt}{24}%
    \or{\@xxvpt}{30}\or{\@xxvpt}{30}}%
  \normalsize \linespacing=\baselineskip
}
\DeclareOption{8pt}{\def\@mainsize{8}\def\@ptsize{8}%
  \def\@typesizes{%
    \or{5}{6}\or{5}{6}\or{5}{6}\or{6}{7}\or{7}{8}%
    \or{8}{10}% normalsize
    \or{9}{11}\or{10}{12}\or{\@xipt}{13}%
    \or{\@xiipt}{14}\or{\@xivpt}{17}}%
  \normalsize \linespacing=\baselineskip
}
\DeclareOption{9pt}{\def\@mainsize{9}\def\@ptsize{9}%
  \def\@typesizes{%
    \or{5}{6}\or{5}{6}\or{6}{7}\or{7}{8}\or{8}{10}%
    \or{9}{11}% normalsize
    \or{10}{12}\or{\@xipt}{13}\or{\@xiipt}{14}%
    \or{\@xivpt}{17}\or{\@xviipt}{20}}%
  \normalsize \linespacing=\baselineskip
}
\def\ps@empty{\let\@mkboth\@gobbletwo
  \let\@oddhead\@empty \let\@evenhead\@empty
  \let\@oddfoot\@empty \let\@evenfoot\@empty
  \global\topskip\normaltopskip}
\def\ps@plain{\ps@empty
  \def\@oddfoot{\normalfont\scriptsize \hfil\thepage\hfil}%
  \let\@evenfoot\@oddfoot}
\let\sectionname\@empty
\let\subsectionname\@empty
\let\subsubsectionname\@empty
\let\paragraphname\@empty
\let\subparagraphname\@empty
\def\leftmark{\expandafter\@firstoftwo\topmark{}{}}
\def\rightmark{\expandafter\@secondoftwo\botmark{}{}}
\long\def\@nilgobble#1\@nil{}
\def\markboth#1#2{%
  \begingroup
    \@temptokena{{#1}{#2}}\xdef\@themark{\the\@temptokena}%
    \mark{\the\@temptokena}%
  \endgroup
  \if@nobreak\ifvmode\nobreak\fi\fi}
\def\ps@myheadings{\ps@headings \let\@mkboth\@gobbletwo}
\newskip\normaltopskip
\normaltopskip=10pt \relax
\let\sectionmark\@gobble
\let\subsectionmark\@gobble
\let\subsubsectionmark\@gobble
\let\paragraphmark\@gobble
\DeclareOption{makeidx}{}
%%%%%%%%%%%%%%%%%%%%%% End of amsart options

%%%%%%%%%%%%%%%%%%%%%% amsgen.sty code
\providecommand{\@saveprimitive}[2]{\begingroup\escapechar`\\\relax
  \edef\@tempa{\string#1}\edef\@tempb{\meaning#1}%
  \ifx\@tempa\@tempb \global\let#2#1%
  \else
    \edef\@tempb{\meaning#2}%
    \ifx\@tempa\@tempb
    \else
      \@latex@error{Unable to properly define \string#2; primitive
      \noexpand#1no longer primitive}\@eha
    \fi
  \fi
  \endgroup}
\let\@xp=\expandafter
\let\@nx=\noexpand
\newtoks\@emptytoks
\def\@oparg#1[#2]{\@ifnextchar[{#1}{#1[#2]}}
\long\def\@ifempty#1{\@xifempty#1@@..\@nil}
\long\def\@xifempty#1#2@#3#4#5\@nil{%
  \ifx#3#4\@xp\@firstoftwo\else\@xp\@secondoftwo\fi}
\long\def\@ifnotempty#1{\@ifempty{#1}{}}
\def\FN@{\futurelet\@let@token}
\def\DN@{\def\next@}
\def\RIfM@{\relax\ifmmode}
\def\setboxz@h{\setbox\z@\hbox}
\def\wdz@{\wd\z@}
\def\boxz@{\box\z@}
\def\relaxnext@{\let\@let@token\relax}
\def\new@ifnextchar#1#2#3{%
  \let\@tempe #1\def\@tempa{#2}\def\@tempb{#3}\futurelet
    \@tempc\new@ifnch}
\def\new@ifnch{\ifx\@tempc \@tempe \let\@tempd\@tempa
             \else\let\@tempd\@tempb\fi\@tempd}
\def\@ifstar#1#2{\new@ifnextchar *{\def\@tempa*{#1}\@tempa}{#2}}
\@ifundefined{every@math@size}{%
\let\every@math@size=\every@size
\def\glb@settings{%
     \expandafter\ifx\csname S@\f@size\endcsname\relax
       \calculate@math@sizes
     \fi
     \csname S@\f@size\endcsname
      \ifmath@fonts
        \begingroup
          \escapechar\m@ne
          \csname mv@\math@version \endcsname
          \globaldefs\@ne
          \let \glb@currsize \f@size
          \math@fonts
        \endgroup
        \the\every@math@size
      \else
      \fi
}
\def\set@fontsize#1#2#3{%
    \@defaultunits\@tempdimb#2pt\relax\@nnil
    \edef\f@size{\strip@pt\@tempdimb}%
    \@defaultunits\@tempskipa#3pt\relax\@nnil
    \edef\f@baselineskip{\the\@tempskipa}%
    \edef\f@linespread{#1}%
    \let\baselinestretch\f@linespread
      \def\size@update{%
        \baselineskip\f@baselineskip\relax
        \baselineskip\f@linespread\baselineskip
        \normalbaselineskip\baselineskip
        \setbox\strutbox\hbox{%
          \vrule\@height.7\baselineskip
                \@depth.3\baselineskip
                \@width\z@}%
%%%     \the\every@size
        \let\size@update\relax}%
  }
}{}% end \@ifundefined test
\newdimen\ex@
\addto@hook\every@math@size{\compute@ex@}
\def\compute@ex@{%
  \begingroup
  \dimen@-\f@size\p@
  \ifdim\dimen@<-20\p@
    \global\ex@ 1.5\p@
  \else
    \advance\dimen@10\p@ \multiply\dimen@\tw@
    \edef\@tempa{\ifdim\dimen@>\z@ -\fi}%
    \dimen@ \ifdim\dimen@<\z@ -\fi \dimen@
    \advance\dimen@-\@m sp % fudge factor
    \vfuzz\p@
    \def\do{\ifdim\dimen@>\z@
      \vfuzz=.97\vfuzz
      \advance\dimen@ -\p@
      \@xp\do \fi}%
    \do
    \dimen@\p@ \advance\dimen@-\vfuzz
    \global\ex@\p@
    \global\advance\ex@ \@tempa\dimen@
  \fi
  \endgroup
}
\def\@addpunct#1{\ifnum\spacefactor>\@m \else#1\fi}
\def\frenchspacing{\sfcode`\.1006\sfcode`\?1005\sfcode`\!1004%
  \sfcode`\:1003\sfcode`\;1002\sfcode`\,1001 }
\def\@mathmeasure#1#2#3{\setbox#1\hbox{\frozen@everymath\@emptytoks
  \m@th$#2#3$}}
\def\nomath@env{\@amsmath@err{%
  \string\begin{\@currenvir} allowed only in paragraph mode%
}\@ehb% "You've lost some text"
}
\def\Invalid@@{Invalid use of \string}
%%%%%%%%%%%%%%%%%%% End amsgen.sty code

\ExecuteOptions{centertags,letterpaper,portrait,%
  10pt,twoside,onecolumn,final}

% Do not allow compatibility mode --- enabling code deleted

%%%%%%%%%%%%%%%%%%%%% amsmath.sty code
\DeclareOption{intlimits}{\let\ilimits@\displaylimits}
\DeclareOption{nointlimits}{\let\ilimits@\nolimits}
\DeclareOption{sumlimits}{\let\slimits@\displaylimits}
\DeclareOption{nosumlimits}{\let\slimits@\nolimits}
\newif\ifctagsplit@
\newif\iftagsleft@
\DeclareOption{leqno}{\tagsleft@true}
\DeclareOption{reqno}{\tagsleft@false}
% add for eqnarray in asl
\AtBeginDocument{\iftagsleft@%
\renewcommand\@eqnnum{\hb@xt@.01\p@{}%   
                      \rlap{\normalfont\normalcolor
                        \hskip -\displaywidth(%
%\theequation    ynm for tagging
\ifasl@tag\tagsymbol\global\asl@tagfalse\else\theequation\fi%
)}}\fi} 

\DeclareOption{centertags}{\ctagsplit@true}
\DeclareOption{tbtags}{\ctagsplit@false}
\DeclareOption{cmex10}{%
  \ifnum\cmex@opt=\@ne \def\cmex@opt{0}%
  \else \def\cmex@opt{10}\fi
}
\@ifundefined{cmex@opt}{\def\cmex@opt{7}}{}
%
\newif\if@fleqn
\newskip\@mathmargin
\@mathmargin\@centering
\DeclareOption{fleqn}{%
    \@fleqntrue
    \@mathmargin = -1sp
    \AtBeginDocument{%
        \ifdim\@mathmargin= -1sp
            \@mathmargin\leftmargini
        \fi
    }%
}
\ExecuteOptions{nointlimits,sumlimits,namelimits,centertags}
\ifnum\cmex@opt=7 \relax
  \catcode`\ =9
  \DeclareFontShape{OMX}{cmex}{m}{n}{%
    <-8> cmex7%
    <8> cmex8%
    <9> cmex9%
    <10> <10.95> <12> <14.4> <17.28> <20.74> <24.88>cmex10%
  }{}%
  \expandafter\let\csname OMX/cmex/m/n/10\endcsname\relax
  \catcode`\ =10
\else
  \ifnum\cmex@opt=\z@ % need to override cmex7 fontdef from amsfonts
    \input{OMXcmex.fd}%
    \expandafter\let\csname OMX/cmex/m/n/10\endcsname\relax
    \def\cmex@opt{10}%
  \fi
\fi

%%%%%%%%%%%%%%%%%%%%%% code from amstext.sty
\DeclareRobustCommand{\text}{%
  \ifmmode\expandafter\text@\else\expandafter\mbox\fi}
\let\nfss@text\text
\def\text@#1{\mathchoice
  {\textdef@\displaystyle\f@size{#1}}%
  {\textdef@\textstyle\tf@size{\firstchoice@false #1}}%
  {\textdef@\textstyle\sf@size{\firstchoice@false #1}}%
  {\textdef@\textstyle \ssf@size{\firstchoice@false #1}}%
  \check@mathfonts
}
\def\textdef@#1#2#3{\hbox{{%
                    \everymath{#1}%
                    \let\f@size#2\selectfont
                    #3}}}
\newif\iffirstchoice@
\firstchoice@true
\def\stepcounter#1{%
  \iffirstchoice@
     \addtocounter{#1}\@ne
     \begingroup \let\@elt\@stpelt \csname cl@#1\endcsname \endgroup
  \fi
}
\def\addtocounter#1#2{%
  \iffirstchoice@
  \@ifundefined {c@#1}{\@nocounterr {#1}}%
    {\global \advance \csname c@#1\endcsname #2\relax}%
  \fi}
\newif\ifmeasuring@
\let\m@gobble\@empty
\@xp\let\csname m@gobble4\endcsname\@gobblefour
\long\@xp\def\csname m@gobble6\endcsname#1#2#3#4#5#6{}
\toks@{%
  \csname m@gobble\iffirstchoice@\ifmeasuring@ 4\fi\else 4\fi\endcsname
  \protect}
\edef\GenericInfo{\the\toks@
  \@xp\@nx\csname GenericInfo \endcsname}
\edef\GenericWarning{\the\toks@
  \@xp\@nx\csname GenericWarning \endcsname}
\toks@{%
  \csname m@gobble\iffirstchoice@\ifmeasuring@ 6\fi\else 6\fi\endcsname
  \protect}
\edef\GenericError{\the\toks@
  \@xp\@nx\csname GenericError \endcsname}
\begingroup \catcode`\"=12
\gdef\mathhexbox#1#2#3{\text{$\m@th\mathchar"#1#2#3$}}
\endgroup
%%%%%%%%%%%%%%%%%%%%%%% end of amstext.sty code

%%%%%%%%%%%%%%%%%%%%%%%%%%%% code from amsbsy.sty
\DeclareRobustCommand{\boldsymbol}[1]{%
  \begingroup
  \let\@nomath\@gobble \mathversion{bold}%
  \math@atom{#1}{%
  \mathchoice%
    {\hbox{$\m@th\displaystyle#1$}}%
    {\hbox{$\m@th\textstyle#1$}}%
    {\hbox{$\m@th\scriptstyle#1$}}%
    {\hbox{$\m@th\scriptscriptstyle#1$}}}%
  \endgroup}
\def\math@atom#1#2{%
   \binrel@{#1}\binrel@@{#2}}
\DeclareRobustCommand{\pmb}{%
  \ifmmode\else \expandafter\pmb@@\fi\mathpalette\pmb@}
\def\pmb@@#1#2#3{\leavevmode\setboxz@h{#3}%
   \dimen@-\wdz@
   \kern-.5\ex@\copy\z@
   \kern\dimen@\kern.25\ex@\raise.4\ex@\copy\z@
   \kern\dimen@\kern.25\ex@\box\z@
}
\newdimen\pmbraise@
\def\pmb@#1#2{\setbox8\hbox{$\m@th#1{#2}$}%
  \setboxz@h{$\m@th#1\mkern.5mu$}\pmbraise@\wdz@
  \binrel@{#2}%
  \dimen@-\wd8 %
  \binrel@@{%
    \mkern-.8mu\copy8 %
    \kern\dimen@\mkern.4mu\raise\pmbraise@\copy8 %
    \kern\dimen@\mkern.4mu\box8 }%
}
\def\binrel@#1{\begingroup
  \setboxz@h{\thinmuskip0mu
    \medmuskip\m@ne mu\thickmuskip\@ne mu
    \setbox\tw@\hbox{$#1\m@th$}\kern-\wd\tw@
    ${}#1{}\m@th$}%
  \edef\@tempa{\endgroup\let\noexpand\binrel@@
    \ifdim\wdz@<\z@ \mathbin
    \else\ifdim\wdz@>\z@ \mathrel
    \else \relax\fi\fi}%
  \@tempa
}
\let\binrel@@\relax
%%%%%%%%%%%%%%%%%%%%%%%% end of amsbsy.sty code

%%%%%%%%%%%%%%%%%%%%% code from amsopn.sty
\@ifundefined{DN@}{%
  \def\FN@{\futurelet\@let@token}%
  \def\DN@{\def\next@}%
}{}
\def\nolimits@{%
  \DN@{\nolimits\ifx\@let@token\limits\expandafter\@gobble\fi}%
  \FN@\next@}
\begingroup \catcode`\"=12
\gdef\newmcodes@{\mathcode`\'39\mathcode`\*42\mathcode`\."613A%
 \mathcode`\-45\mathcode`\/47\mathcode`\:"603A\relax}
\endgroup
\DeclareRobustCommand{\operatorname}{%
  \@ifstar{\qopname\newmcodes@ m}%
          {\qopname\newmcodes@ o}}%
\DeclareRobustCommand{\qopname}[3]{%
  \mathop{#1\kern\z@\operator@font#3}%
  \csname n#2limits@\endcsname}
\newcommand{\DeclareMathOperator}{%
  \@ifstar{\@declmathop\@empty}{\@declmathop o}}
\long\def\@declmathop#1#2#3{%
  \@ifdefinable{#2}{%
    \DeclareRobustCommand{#2}{\qopname\newmcodes@#1{#3}}}}
\@onlypreamble\DeclareMathOperator
\@onlypreamble\@declmathop
\def\arccos{\qopname\relax o{arccos}}
\def\arcsin{\qopname\relax o{arcsin}}
\def\arctan{\qopname\relax o{arctan}}
\def\arg{\qopname\relax o{arg}}
\def\cos{\qopname\relax o{cos}}
\def\cosh{\qopname\relax o{cosh}}
\def\cot{\qopname\relax o{cot}}
\def\coth{\qopname\relax o{coth}}
\def\csc{\qopname\relax o{csc}}
\def\deg{\qopname\relax o{deg}}
\def\det{\qopname\relax\@empty{det}}
\def\dim{\qopname\relax o{dim}}
\def\exp{\qopname\relax o{exp}}
\def\gcd{\qopname\relax\@empty{gcd}}
\def\hom{\qopname\relax o{hom}}
\def\inf{\qopname\relax\@empty{inf}}
\def\injlim{\qopname\relax\@empty{inj\,lim}}
\def\ker{\qopname\relax o{ker}}
\def\lg{\qopname\relax o{lg}}
\def\lim{\qopname\relax\@empty{lim}}
\def\liminf{\qopname\relax\@empty{lim\,inf}}
\def\limsup{\qopname\relax\@empty{lim\,sup}}
\def\ln{\qopname\relax o{ln}}
\def\log{\qopname\relax o{log}}
\def\max{\qopname\relax\@empty{max}}
\def\min{\qopname\relax\@empty{min}}
\def\Pr{\qopname\relax\@empty{Pr}}
\def\projlim{\qopname\relax\@empty{proj\,lim}}
\def\sec{\qopname\relax o{sec}}
\def\sin{\qopname\relax o{sin}}
\def\sinh{\qopname\relax o{sinh}}
\def\sup{\qopname\relax\@empty{sup}}
\def\tan{\qopname\relax o{tan}}
\def\tanh{\qopname\relax o{tanh}}
\def\operator@font{\mathgroup\symoperators}
\def\operatornamewithlimits{\operatorname*}
\def\varlim@#1#2{\mathop{\vtop{\ialign{##\crcr
 \hfil$#1\m@th\operator@font lim$\hfil\crcr
 \noalign{\nointerlineskip\kern\ex@}#2#1\crcr
 \noalign{\nointerlineskip\kern-\ex@}\crcr}}}}
\def\varinjlim{\mathpalette\varlim@\rightarrowfill@}
\def\varprojlim{\mathpalette\varlim@\leftarrowfill@}
\def\varliminf{\mathpalette\varliminf@{}}
\def\varliminf@#1{\mathop{\@@underline{\vrule\@depth.2\ex@\@width\z@
   \hbox{$#1\m@th\operator@font lim$}}}}
\def\varlimsup{\mathpalette\varlimsup@{}}
\def\varlimsup@#1{\mathop{\@@overline
  {\hbox{$#1\m@th\operator@font lim$}}}}
\DeclareOption{namelimits}{\let\nmlimits@\displaylimits}
\DeclareOption{nonamelimits}{\let\nmlimits@\nolimits}
%%%%%%%%%%%%%%%%%%%%%%%%%% end of code from amsopn.sty

\def\@amsmath@err{\PackageError{amsmath}}
\def\AmS{{\protect\AmSfont
  A\kern-.1667em\lower.5ex\hbox{M}\kern-.125emS}}
\def\AmSfont{%
  \usefont{OMS}{cmsy}{\if\@xp\@car\f@series\@nil bb\else m\fi}{n}}
\def\pr@m@s{%
  \ifx\@let@token'\DN@##1{\prim@s}\else\let\next@\egroup\fi\next@}
\def\prim@s{\prime\futurelet\@let@token\pr@m@s}
\let\@prime=\prime
\renewcommand{\prime}{{\kern\z@\@prime}}
\DeclareRobustCommand{\tmspace}[3]{%
  \ifmmode\mskip#1#2\else\kern#1#3\fi\relax}
\renewcommand{\,}{\tmspace+\thinmuskip{.1667em}}
\let\thinspace\,
\renewcommand{\!}{\tmspace-\thinmuskip{.1667em}}
\let\negthinspace\!
\renewcommand{\:}{\tmspace+\medmuskip{.2222em}}
\let\medspace\:
\newcommand{\negmedspace}{\tmspace-\medmuskip{.2222em}}
\renewcommand{\;}{\tmspace+\thickmuskip{.2777em}}
\let\thickspace\;
\newcommand{\negthickspace}{\tmspace-\thickmuskip{.2777em}}
\newcommand{\mspace}[1]{\mskip#1\relax}
\begingroup\catcode`\"=12
\def\@tempa#1{\expandafter\@tempb\meaning#1%
\relax\relax\relax\relax"0000\@nil#1}
\def\@tempb#1"#2#3#4#5#6\@nil#7{%
  \ifnum"#2=7 \count@"1#3#4#5\relax
    \ifnum\count@<"1000 \else \global\mathchardef#7="0#3#4#5\relax \fi
  \fi}
\@tempa\Gamma \@tempa\Delta \@tempa\Theta \@tempa\Lambda \@tempa\Xi
\@tempa\Pi \@tempa\Sigma \@tempa\Upsilon \@tempa\Phi \@tempa\Psi
\@tempa\Omega
\@ifundefined{varGamma}{%
  \DeclareMathSymbol{\varGamma}{\mathord}{letters}{"00}
  \DeclareMathSymbol{\varDelta}{\mathord}{letters}{"01}
  \DeclareMathSymbol{\varTheta}{\mathord}{letters}{"02}
  \DeclareMathSymbol{\varLambda}{\mathord}{letters}{"03}
  \DeclareMathSymbol{\varXi}{\mathord}{letters}{"04}
  \DeclareMathSymbol{\varPi}{\mathord}{letters}{"05}
  \DeclareMathSymbol{\varSigma}{\mathord}{letters}{"06}
  \DeclareMathSymbol{\varUpsilon}{\mathord}{letters}{"07}
  \DeclareMathSymbol{\varPhi}{\mathord}{letters}{"08}
  \DeclareMathSymbol{\varPsi}{\mathord}{letters}{"09}
  \DeclareMathSymbol{\varOmega}{\mathord}{letters}{"0A}
}{}
\endgroup
\begingroup \catcode`\"=12
\gdef\@@sqrt#1{\radical"270370 {#1}}
\endgroup
\@saveprimitive\overline\@@overline
\def\overline#1{\@@overline{#1}}
\def\boxed#1{\fbox{\m@th$\displaystyle#1$}}
\def\implies{\DOTSB\;\Longrightarrow\;}
\def\impliedby{\DOTSB\;\Longleftarrow\;}
\begingroup \catcode`\"=12 % in case activated by a preceding package
\gdef\And{\DOTSB\;\mathchar"3026 \;}
\@tempcnta=\@xp\@gobble\vert \advance\@tempcnta "4000000
\xdef\lvert{\delimiter\number\@tempcnta\space }
\advance\@tempcnta "1000000
\xdef\rvert{\delimiter\number\@tempcnta\space }
\@tempcnta=\@xp\@gobble\Vert \advance\@tempcnta "4000000
\xdef\lVert{\delimiter\number\@tempcnta\space }
\advance\@tempcnta "1000000
\xdef\rVert{\delimiter\number\@tempcnta\space }
\endgroup % restore "
\@saveprimitive\over\@@over
\@saveprimitive\atop\@@atop
\@saveprimitive\above\@@above
\@saveprimitive\overwithdelims\@@overwithdelims
\@saveprimitive\atopwithdelims\@@atopwithdelims
\@saveprimitive\abovewithdelims\@@abovewithdelims
\DeclareRobustCommand{\primfrac}[1]{%
  \PackageWarning{amsmath}{%
Foreign command \@backslashchar#1; %
\protect\frac\space or \protect\genfrac\space should be used instead%
  }
  \global\@xp\let\csname#1\@xp\endcsname\csname @@#1\endcsname
  \csname#1\endcsname
}
\renewcommand{\over}{\primfrac{over}}
\renewcommand{\atop}{\primfrac{atop}}
\renewcommand{\above}{\primfrac{above}}
\renewcommand{\overwithdelims}{\primfrac{overwithdelims}}
\renewcommand{\atopwithdelims}{\primfrac{atopwithdelims}}
\renewcommand{\abovewithdelims}{\primfrac{abovewithdelims}}
\DeclareRobustCommand{\frac}[2]{{\begingroup#1\endgroup\@@over#2}}
\newcommand{\dfrac}{\genfrac{}{}{}0}
\newcommand{\tfrac}{\genfrac{}{}{}1}
\DeclareRobustCommand{\binom}{\genfrac()\z@{}}
\newcommand{\dbinom}{\genfrac(){0pt}0}
\newcommand{\tbinom}{\genfrac(){0pt}1}
\DeclareRobustCommand{\genfrac}[4]{%
  \def\@tempa{#1#2}%
  \edef\@tempb{\@nx\@genfrac\@mathstyle{#4}%
    \csname @@\ifx @#3@over\else above\fi
    \ifx\@tempa\@empty \else withdelims\fi\endcsname}
  \@tempb{#1#2#3}}
\def\@genfrac#1#2#3#4#5{{#1{\begingroup#4\endgroup#2#3\relax#5}}}
\def\@mathstyle#1{%
  \ifx\@empty#1\@empty\relax
  \else\ifcase#1\displaystyle % case 0
    \or\textstyle\or\scriptstyle\else\scriptscriptstyle\fi\fi}
\def\colon{\nobreak\mskip2mu\mathpunct{}\nonscript
  \mkern-\thinmuskip{:}\mskip6muplus1mu\relax}
\begingroup \catcode`\"=12
\edef\@tempa{\string\mathchar"}
\def\@tempb#1"#2\@nil{#1"}
\edef\@tempc{\expandafter\@tempb\meaning\coprod "\@nil}
\ifx\@tempa\@tempc
  \global\let\coprod@\coprod
  \gdef\coprod{\DOTSB\coprod@\slimits@}
  \global\let\bigvee@\bigvee
  \gdef\bigvee{\DOTSB\bigvee@\slimits@}
  \global\let\bigwedge@\bigwedge
  \gdef\bigwedge{\DOTSB\bigwedge@\slimits@}
  \global\let\biguplus@\biguplus
  \gdef\biguplus{\DOTSB\biguplus@\slimits@}
  \global\let\bigcap@\bigcap
  \gdef\bigcap{\DOTSB\bigcap@\slimits@}
  \global\let\bigcup@\bigcup
  \gdef\bigcup{\DOTSB\bigcup@\slimits@}
  \global\let\prod@\prod
  \gdef\prod{\DOTSB\prod@\slimits@}
  \global\let\sum@\sum
  \gdef\sum{\DOTSB\sum@\slimits@}
  \global\let\bigotimes@\bigotimes
  \gdef\bigotimes{\DOTSB\bigotimes@\slimits@}
  \global\let\bigoplus@\bigoplus
  \gdef\bigoplus{\DOTSB\bigoplus@\slimits@}
  \global\let\bigodot@\bigodot
  \gdef\bigodot{\DOTSB\bigodot@\slimits@}
  \global\let\bigsqcup@\bigsqcup
  \gdef\bigsqcup{\DOTSB\bigsqcup@\slimits@}
\fi
\endgroup
\newcommand{\nobreakdash}{\leavevmode
  \toks@\@emptytoks \def\@tempa##1{\toks@\@xp{\the\toks@-}\FN@\next@}%
  \DN@{\ifx\@let@token-\@xp\@tempa
       \else\setboxz@h{\the\toks@\nobreak}\unhbox\z@\fi}%
  \FN@\next@
}
\def\leftroot{\@amsmath@err{\Invalid@@\leftroot}\@eha}
\def\uproot{\@amsmath@err{\Invalid@@\uproot}\@eha}
\newcount\uproot@
\newcount\leftroot@
\def\root{\relaxnext@
  \DN@{\ifx\@let@token\uproot\let\next@\nextii@\else
   \ifx\@let@token\leftroot\let\next@\nextiii@\else
   \let\next@\plainroot@\fi\fi\next@}%
  \def\nextii@\uproot##1{\uproot@##1\relax\FN@\nextiv@}%
  \def\nextiv@{\ifx\@let@token\@sptoken\DN@. {\FN@\nextv@}\else
   \DN@.{\FN@\nextv@}\fi\next@.}%
  \def\nextv@{\ifx\@let@token\leftroot\let\next@\nextvi@\else
   \let\next@\plainroot@\fi\next@}%
  \def\nextvi@\leftroot##1{\leftroot@##1\relax\plainroot@}%
   \def\nextiii@\leftroot##1{\leftroot@##1\relax\FN@\nextvii@}%
  \def\nextvii@{\ifx\@let@token\@sptoken
   \DN@. {\FN@\nextviii@}\else
   \DN@.{\FN@\nextviii@}\fi\next@.}%
  \def\nextviii@{\ifx\@let@token\uproot\let\next@\nextix@\else
   \let\next@\plainroot@\fi\next@}%
  \def\nextix@\uproot##1{\uproot@##1\relax\plainroot@}%
  \bgroup\uproot@\z@\leftroot@\z@\FN@\next@}
\def\plainroot@#1\of#2{\setbox\rootbox\hbox{%
 $\m@th\scriptscriptstyle{#1}$}%
 \mathchoice{\r@@t\displaystyle{#2}}{\r@@t\textstyle{#2}}
 {\r@@t\scriptstyle{#2}}{\r@@t\scriptscriptstyle{#2}}\egroup}
\def\r@@t#1#2{\setboxz@h{$\m@th#1\@@sqrt{#2}$}%
 \dimen@\ht\z@\advance\dimen@-\dp\z@
 \setbox\@ne\hbox{$\m@th#1\mskip\uproot@ mu$}%
 \advance\dimen@ by1.667\wd\@ne
 \mkern-\leftroot@ mu\mkern5mu\raise.6\dimen@\copy\rootbox
 \mkern-10mu\mkern\leftroot@ mu\boxz@}
\let\ifgtest@\iffalse                              % initial value
\def\gtest@true{\global\let\ifgtest@\iftrue}
\def\gtest@false{\global\let\ifgtest@\iffalse}
\let\DOTSI\relax
\let\DOTSB\relax
\let\DOTSX\relax
{\uccode`7=`\\ \uccode`8=`m \uccode`9=`a \uccode`0=`t \uccode`!=`h
 \uppercase{%
  \gdef\math@#1#2#3#4#5#6\math@{\gtest@false\ifx 7#1\ifx 8#2%
  \ifx 9#3\ifx 0#4\ifx !#5\xdef\meaning@{#6}\gtest@true
  \fi\fi\fi\fi\fi}}}
{\uccode`7=`c \uccode`8=`h \uccode`9=`\"
 \uppercase{\gdef\mathch@#1#2#3#4#5#6\mathch@{\gtest@false
  \ifx 7#1\ifx 8#2\ifx 9#5\gtest@true\xdef\meaning@{9#6}\fi\fi\fi}}}
\newcount\classnum@
\def\getmathch@#1.#2\getmathch@{\classnum@#1 \divide\classnum@4096
 \ifcase\number\classnum@\or\or\gdef\thedots@{\dotsb@}\or
 \gdef\thedots@{\dotsb@}\fi}
{\uccode`4=`b \uccode`5=`i \uccode`6=`n
 \uppercase{\gdef\mathbin@#1#2#3{\relaxnext@
  \def\nextii@##1\mathbin@{\ifx\@sptoken\@let@token\gtest@true\fi}%
  \gtest@false\DN@##1\mathbin@{}%
 \ifx 4#1\ifx 5#2\ifx 6#3\DN@{\FN@\nextii@}\fi\fi\fi\next@}}}
{\uccode`4=`r \uccode`5=`e \uccode`6=`l
 \uppercase{\gdef\mathrel@#1#2#3{\relaxnext@
  \def\nextii@##1\mathrel@{\ifx\@sptoken\@let@token\gtest@true\fi}%
 \gtest@false\DN@##1\mathrel@{}%
 \ifx 4#1\ifx 5#2\ifx 6#3\DN@{\FN@\nextii@}\fi\fi\fi\next@}}}
{\uccode`5=`m \uccode`6=`a \uccode`7=`c
 \uppercase{\gdef\macro@#1#2#3#4\macro@{\gtest@false
  \ifx 5#1\ifx 6#2\ifx 7#3\gtest@true
  \xdef\meaning@{\macro@@#4\macro@@}\fi\fi\fi}}}
\def\macro@@#1->#2\macro@@{#2}
\newcount\DOTSCASE@
{\uccode`6=`\\ \uccode`7=`D \uccode`8=`O \uccode`9=`T \uccode`0=`S
 \uppercase{\gdef\DOTS@#1#2#3#4#5{\gtest@false\DN@##1\DOTS@{}%
  \ifx 6#1\ifx 7#2\ifx 8#3\ifx 9#4\ifx 0#5\let\next@\DOTS@@
  \fi\fi\fi\fi\fi
  \next@}}}
{\uccode`3=`B \uccode`4=`I \uccode`5=`X
 \uppercase{\gdef\DOTS@@#1{\relaxnext@
  \def\nextii@##1\DOTS@{\ifx\@sptoken\@let@token\gtest@true\fi}%
  \DN@{\FN@\nextii@}%
  \ifx 3#1\global\DOTSCASE@\z@\else
  \ifx 4#1\global\DOTSCASE@\@ne\else
  \ifx 5#1\global\DOTSCASE@\tw@\else\DN@##1\DOTS@{}%
  \fi\fi\fi\next@}}}
{\uccode`5=`\\ \uccode`6=`n \uccode`7=`o \uccode`8=`t
 \uppercase{\gdef\not@#1#2#3#4{\relaxnext@
  \def\nextii@##1\not@{\ifx\@sptoken\@let@token\gtest@true\fi}%
 \gtest@false\DN@##1\not@{}%
 \ifx 5#1\ifx 6#2\ifx 7#3\ifx 8#4\DN@{\FN@\nextii@}\fi\fi\fi
 \fi\next@}}}
\def\keybin@{\gtest@true
 \ifx\@let@token+\else\ifx\@let@token=\else
 \ifx\@let@token<\else\ifx\@let@token>\else
 \ifx\@let@token-\else\ifx\@let@token*\else\ifx\@let@token:\else
   \gtest@false\fi\fi\fi\fi\fi\fi\fi}
\@ifundefined{@ldots}{\def\@ldots{\mathellipsis}}{}
\DeclareRobustCommand{\dots}{\relax
  \csname\ifmmode m\else t\fi dots@\endcsname}
\def\tdots@{\leavevmode\unskip\relaxnext@
 \DN@{$\m@th\@ldots\,
   \ifx\@let@token,\,$\else\ifx\@let@token.\,$\else
   \ifx\@let@token;\,$\else\ifx\@let@token:\,$\else
   \ifx\@let@token?\,$\else\ifx\@let@token!\,$\else
     $ \fi\fi\fi\fi\fi\fi}%
  \ \FN@\next@}
\def\mdots@{\FN@\mdots@@}
\def\mdots@@{\gdef\thedots@{\dotso@}%
 \ifx\@let@token\boldsymbol \gdef\thedots@\boldsymbol{\boldsymboldots@}%
 \else\ifx,\@let@token \gdef\thedots@{\dotsc}%
 \else\ifx\not\@let@token \gdef\thedots@{\dotsb@}%
 \else\keybin@
 \ifgtest@\gdef\thedots@{\dotsb@}%
 \else\xdef\meaning@{\meaning\@let@token..........}%
   \xdef\meaning@@{\meaning@}%
  \@xp\math@\meaning@\math@
  \ifgtest@
   \@xp\mathch@\meaning@\mathch@
   \ifgtest@\@xp\getmathch@\meaning@\getmathch@\fi
  \else\@xp\macro@\meaning@@\macro@
  \ifgtest@
   \@xp\not@\meaning@\not@\ifgtest@\gdef\thedots@{\dotsb@}%
  \else\@xp\DOTS@\meaning@\DOTS@
  \ifgtest@
   \ifcase\number\DOTSCASE@\gdef\thedots@{\dotsb@}%
    \or\gdef\thedots@{\dotsi}\else\fi
  \else\@xp\math@\meaning@\math@
  \ifgtest@\@xp\mathbin@\meaning@\mathbin@
  \ifgtest@\gdef\thedots@{\dotsb@}%
  \else\@xp\mathrel@\meaning@\mathrel@
  \ifgtest@\gdef\thedots@{\dotsb@}%
  \fi\fi\fi\fi\fi\fi\fi\fi\fi\fi\fi
 \thedots@}
\def\boldsymboldots@#1{\bold@true\let\@let@token=#1\let\delayed@=#1\mdots@@
 \boldsymbol#1\bold@false}
\def\@cdots{\mathinner{\cdotp\cdotp\cdotp}}
\def\dotsi{\!\@cdots}
\let\dotsb@\@cdots
\def\rightdelim@{\gtest@true
 \ifx\@let@token)\else
 \ifx\@let@token]\else
 \ifx\@let@token\rbrack\else
 \ifx\@let@token\}\else
 \ifx\@let@token\rbrace\else
 \ifx\@let@token\rangle\else
 \ifx\@let@token\rceil\else
 \ifx\@let@token\rfloor\else
 \ifx\@let@token\rgroup\else
 \ifx\@let@token\rmoustache\else
 \ifx\@let@token\right\else
 \ifx\@let@token\bigr\else
 \ifx\@let@token\biggr\else
 \ifx\@let@token\Bigr\else
 \ifx\@let@token\Biggr\else\gtest@false
 \fi\fi\fi\fi\fi\fi\fi\fi\fi\fi\fi\fi\fi\fi\fi}
\def\extra@{%
 \rightdelim@\ifgtest@
 \else\ifx\@let@token$\gtest@true
 \else\xdef\meaning@{\meaning\@let@token..........}%
 \@xp\macro@\meaning@\macro@\ifgtest@
 \@xp\DOTS@\meaning@\DOTS@
 \ifgtest@
 \ifnum\DOTSCASE@=\tw@\gtest@true\else\gtest@false
 \fi\fi\fi\fi\fi}
\newif\ifbold@
\def\dotso@{\relaxnext@
 \ifbold@
  \let\@let@token\delayed@
  \def\nextii@{\extra@\@ldots\ifgtest@\,\fi}%
 \else
  \def\nextii@{\DN@{\extra@\@ldots\ifgtest@\,\fi}\FN@\next@}%
 \fi
 \nextii@}
\def\extrap@#1{%
 \DN@{#1\,}%
 \ifx\@let@token,\else
 \ifx\@let@token;\else
 \ifx\@let@token.\else\extra@
 \ifgtest@\else
 \let\next@#1\fi\fi\fi\fi\next@}
\DeclareRobustCommand{\ldots}{\relax
  \ifmmode \DN@{\extrap@\@ldots}%
  \else \let\next@\tdots@\fi
  \FN@\next@}
\DeclareRobustCommand{\cdots}{\DN@{\extrap@\@cdots}\FN@\next@}
\let\dotso\ldots
\let\dotsb\cdots
\let\dotsm\dotsb
\DeclareRobustCommand{\dotsc}{%
  \DN@{\ifx\@let@token;\@ldots\,%
       \else \ifx\@let@token.\@ldots\,%
       \else \extra@\@ldots \ifgtest@\,\fi
       \fi\fi}%
  \FN@\next@}
\def\longrightarrow{\DOTSB\relbar\joinrel\rightarrow}
\def\Longrightarrow{\DOTSB\Relbar\joinrel\Rightarrow}
\def\longleftarrow{\DOTSB\leftarrow\joinrel\relbar}
\def\Longleftarrow{\DOTSB\Leftarrow\joinrel\Relbar}
\def\longleftrightarrow{\DOTSB\leftarrow\joinrel\rightarrow}
\def\Longleftrightarrow{\DOTSB\Leftarrow\joinrel\Rightarrow}
\def\mapsto{\DOTSB\mapstochar\rightarrow}
\def\longmapsto{\DOTSB\mapstochar\longrightarrow}
\def\hookrightarrow{\DOTSB\lhook\joinrel\rightarrow}
\def\hookleftarrow{\DOTSB\leftarrow\joinrel\rhook}
\def\iff{\DOTSB\;\Longleftrightarrow\;}
\def\doteq{\DOTSB\mathrel{\mathop{\kern\z@ =}\limits^{\textstyle.}}}
\newif\if@display
\everydisplay\@xp{\the\everydisplay \@displaytrue}
\def\int{\DOTSI\intop\ilimits@}
\def\oint{\DOTSI\ointop\ilimits@}
\def\intkern@{\mkern-6mu\mathchoice{\mkern-3mu}{}{}{}}
\def\intdots@{\mathchoice{\@cdots}%
 {{\cdotp}\mkern1.5mu{\cdotp}\mkern1.5mu{\cdotp}}%
 {{\cdotp}\mkern1mu{\cdotp}\mkern1mu{\cdotp}}%
 {{\cdotp}\mkern1mu{\cdotp}\mkern1mu{\cdotp}}}
\def\iint{\DOTSI\protect\ints@\tw@}
\def\iiint{\DOTSI\protect\ints@\thr@@}
\def\iiiint{\DOTSI\protect\ints@{4}}
\def\idotsint{\DOTSI\protect\ints@\z@}
\def\ints@#1{%
  \mkern-7mu\mathchoice{\mkern-2mu}{}{}{}%
  \mathop{\mkern7mu\mathchoice{\mkern2mu}{}{}{}%
    \intop\ifnum#1=\z@\intdots@
    \else\intkern@\fi
    \ifnum#1>\tw@\intop\intkern@\fi
    \ifnum#1>\thr@@\intop\intkern@\fi
    \intop
  }\ilimits@
}
\newbox\Mathstrutbox@
\setbox\Mathstrutbox@=\hbox{}
\def\Mathstrut@{\copy\Mathstrutbox@}
\begingroup \catcode`\"=12
\gdef\resetMathstrut@{%
  \setbox\z@\hbox{%
    \mathchardef\@tempa\mathcode`\(\relax
    \def\@tempb##1"##2##3{\the\textfont"##3\char"}%
    \expandafter\@tempb\meaning\@tempa \relax
  }%
  \ht\Mathstrutbox@\ht\z@ \dp\Mathstrutbox@\dp\z@
}
\endgroup
\addto@hook\every@math@size{\resetMathstrut@}
\newbox\strutbox@
\def\strut@{\copy\strutbox@}
\addto@hook\every@math@size{%
  \global\setbox\strutbox@\hbox{\lower.5\normallineskiplimit
         \vbox{\kern-\normallineskiplimit\copy\strutbox}}}
\def\big{\bBigg@\@ne}
\def\Big{\bBigg@{1.5}}
\def\bigg{\bBigg@\tw@}
\def\Bigg{\bBigg@{2.5}}
\def\bBigg@#1#2{%
  {\@mathmeasure\z@{\nulldelimiterspace\z@}%
     {\left#2\vcenter to#1\big@size{}\right.}%
   \box\z@}}
\addto@hook\every@math@size{%
  \global\big@size 1.2\ht\Mathstrutbox@
  \global\advance\big@size 1.2\dp\Mathstrutbox@ }
\newdimen\big@size
\def\accentclass@{7}
\def\noaccents@{\def\accentclass@{0}}
\DeclareFontEncoding{OML}{}{\noaccents@}
\DeclareFontEncoding{OMS}{}{\noaccents@}
\begingroup \catcode`\"=12
\def\@tempa#1#2\@nil{\def\@tempc{#1}}\def\@tempb{\mathaccent}
\expandafter\@tempa\hat \relax\relax\@nil
\ifx\@tempb\@tempc
  \def\@tempa#1\@nil{#1}%
  \def\@tempb#1{\afterassignment\@tempa\mathchardef\@tempc=}%
  \def\@tempe#1"{}
  \def\do#1"#2{}
  \def\@tempd#1#2{\expandafter\@tempb#1\@nil
    \ifnum\@tempc<"1000
      \edef\@tempc{"\@nx\accentclass@
        \ifnum\@tempc<"100 0\fi
        \@xp\@tempe\meaning\@tempc\space}%
    \else
      \edef\@tempc{"\@nx\@nx\@nx\accentclass@
        \@xp\do\meaning\@tempc\space}%
    \fi
    \xdef#1{\mathaccent\@tempc}%
    \toks@{%
      \relax\ifmmode \else\DN@##1##2{\nonmatherr@{#2}}\@xp\next@\fi
      \mathaccent@}%
    \xdef#2{\the\toks@{\@tempc}}%
  }
  \@tempd\hat\Hat \@tempd\check\Check \@tempd\tilde\Tilde
  \@tempd\acute\Acute \@tempd\grave\Grave \@tempd\dot\Dot
  \@tempd\ddot\Ddot \@tempd\breve\Breve \@tempd\bar\Bar
\fi
\endgroup
\newcount\skewcharcount@
\newcount\familycount@
\def\theskewchar@{\familycount@\@ne
 \global\skewcharcount@\the\skewchar\textfont\@ne
 \ifnum\mathgroup>\m@ne\ifnum\mathgroup<16
  \global\familycount@\the\mathgroup\relax
  \global\skewcharcount@\the\skewchar\textfont\the\mathgroup\relax\fi\fi
 \ifnum\skewcharcount@>\m@ne
  \ifnum\skewcharcount@<128
  \multiply\familycount@256
  \global\advance\skewcharcount@\familycount@
  \global\advance\skewcharcount@28672
  \mathchar\skewcharcount@\else
  \global\skewcharcount@\m@ne\fi\else
 \global\skewcharcount@\m@ne\fi}
\newcount\pointcount@
\def\getpoints@#1.#2\getpoints@{\pointcount@#1 }
\newdimen\accentdimen@
\newcount\accentmu@
\def\dimentomu@{\multiply\accentdimen@ 100
 \@xp\getpoints@\the\accentdimen@\getpoints@
 \multiply\pointcount@18
 \divide\pointcount@\@m
 \global\accentmu@\pointcount@}
\def\mathaccent@#1#2{\ifnum\mathgroup=\m@ne\xdef\thefam@{1}\else
 \xdef\thefam@{\the\mathgroup}\fi
 \accentdimen@\z@
 \setboxz@h{\unbracefonts@$\m@th\mathgroup\thefam@\relax#2$}%
 \ifdim\accentdimen@=\z@\DN@{\mathaccent#1{#2}}%
  \setbox\@ne\hbox{\unbracefonts@
    $\m@th\mathgroup\thefam@\relax#2\theskewchar@$}
  \setbox\tw@\hbox{$\m@th\ifnum\skewcharcount@=\m@ne\else
   \mathchar\skewcharcount@\fi$}%
  \global\accentdimen@\wd\@ne\global\advance\accentdimen@-\wdz@
  \global\advance\accentdimen@-\wd\tw@
  \global\multiply\accentdimen@\tw@
  \dimentomu@\global\advance\accentmu@\@ne
 \else\DN@{{\mathaccent#1{#2\mkern\accentmu@ mu}%
    \mkern-\accentmu@ mu}{}}\fi
 \next@}
\def\unbracefonts@{\let\math@bgroup\@empty\let\math@egroup\@empty}
\def\nonmatherr@#1{\@amsmath@err{\protect
  #1 allowed only in math mode}\@ehd}
\begingroup \catcode`\"=12
\def\@tempa#1#2{\gdef#1{\RIfM@\DN@{\mathaccent@{"\accentclass@#2 }}%
  \else\DN@{\nonmatherr@{#1}}\fi\next@}}
\@tempa\Hat{05E}\@tempa\Check{014}\@tempa\Tilde{07E}\@tempa\Acute{013}
\@tempa\Grave{012}\@tempa\Dot{05F}\@tempa\Ddot{07F}\@tempa\Breve{015}
\@tempa\Bar{016}
\gdef\Vec{\RIfM@\DN@{\mathaccent@{"017E }}\else
 \DN@{\nonmatherr@\Vec}\fi\next@}
\endgroup
\def\dddot#1{{\mathop{#1}\limits^{\vbox to-1.4\ex@{\kern-\tw@\ex@
 \hbox{\normalfont ...}\vss}}}}
\def\ddddot#1{{\mathop{#1}\limits^{\vbox to-1.4\ex@{\kern-\tw@\ex@
 \hbox{\normalfont....}\vss}}}}
\def\bmod{\mskip-\medmuskip\mkern5mu\mathbin
  {\operator@font mod}\penalty900
  \mkern5mu\mskip-\medmuskip}
\def\pod#1{\allowbreak\if@display\mkern18mu\else\mkern8mu\fi(#1)}
\def\pmod#1{\pod{{\operator@font mod}\mkern6mu#1}}
\def\mod#1{\allowbreak\if@display\mkern18mu
  \else\mkern12mu\fi{\operator@font mod}\,\,#1}
\newcommand{\cfrac}[3][c]{{\displaystyle\frac{%
  \strut\ifx r#1\hfill\fi#2\ifx l#1\hfill\fi}{#3}}%
  \kern-\nulldelimiterspace}
\def\overset#1#2{\binrel@{#2}%
  \binrel@@{\mathop{\kern\z@#2}\limits^{#1}}}
\def\underset#1#2{\binrel@{#2}%
  \binrel@@{\mathop{\kern\z@#2}\limits_{#1}}}
\def\sideset#1#2#3{%
  \@mathmeasure\z@\displaystyle{#3}%
  \global\setbox\@ne\vbox to\ht\z@{}\dp\@ne\dp\z@
  \setbox\tw@\box\@ne
  \@mathmeasure4\displaystyle{\copy\tw@#1}%
  \@mathmeasure6\displaystyle{#3\nolimits#2}%
  \dimen@-\wd6 \advance\dimen@\wd4 \advance\dimen@\wd\z@
  \hbox to\dimen@{}\mathop{\kern-\dimen@\box4\box6}%
}
\renewcommand{\smash}[2][tb]{%
  \def\smash@{#1}%
  \ifmmode\@xp\mathpalette\@xp\mathsm@sh\else
        \@xp\makesm@sh\fi{#2}}
\def\finsm@sh{\def\mb@t{\ht\z@\z@}\def\mb@b{\dp\z@\z@}%
  \def\mb@tb{\mb@t\mb@b}%
  {\csname mb@\smash@\endcsname}%
  \leavevmode\boxz@}
\def\rightarrowfill@#1{\m@th\setboxz@h{$#1\relbar$}\ht\z@\z@
  $#1\copy\z@\mkern-6mu\cleaders
  \hbox{$#1\mkern-2mu\box\z@\mkern-2mu$}\hfill
  \mkern-6mu\mathord\rightarrow$}
\def\leftarrowfill@#1{\m@th\setboxz@h{$#1\relbar$}\ht\z@\z@
  $#1\mathord\leftarrow\mkern-6mu\cleaders
  \hbox{$#1\mkern-2mu\copy\z@\mkern-2mu$}\hfill
  \mkern-6mu\box\z@$}
\def\leftrightarrowfill@#1{\m@th\setboxz@h{$#1\relbar$}\ht\z@\z@
  $#1\mathord\leftarrow\mkern-6mu\cleaders
  \hbox{$#1\mkern-2mu\box\z@\mkern-2mu$}\hfill
  \mkern-6mu\mathord\rightarrow$}
\def\overarrow@#1#2#3{\vbox{\ialign{##\crcr#1#2\crcr
 \noalign{\kern-\ex@\nointerlineskip}$\m@th\hfil#2#3\hfil$\crcr}}}
\def\overrightarrow{\mathpalette{\overarrow@\rightarrowfill@}}
\def\overleftarrow{\mathpalette{\overarrow@\leftarrowfill@}}
\def\overleftrightarrow{\mathpalette{\overarrow@\leftrightarrowfill@}}
\def\underarrow@#1#2#3{%
 \vtop{\ialign{##\crcr$\m@th\hfil#2#3\hfil$\crcr
 \noalign{\nointerlineskip\kern-.5\ex@}#1#2\crcr}}}
\def\underrightarrow{\mathpalette{\underarrow@\rightarrowfill@}}
\def\underleftarrow{\mathpalette{\underarrow@\leftarrowfill@}}
\def\underleftrightarrow{\mathpalette{\underarrow@\leftrightarrowfill@}}
\newcommand{\xrightarrow}[2][]{%
  \mathrel{\mathop{%
    \setbox\z@\vbox{\m@th
      \hbox{$\scriptstyle\;{#1}\;\;$}%
      \hbox{$\m@th\scriptstyle\;{#2}\;\;$}%
    }%
    \hbox to\ifdim\wd\z@>\minaw@\wd\z@\else\minaw@\fi{%
      \rightarrowfill@\displaystyle}}%
  \limits^{#2}\@ifnotempty{#1}{_{#1}}}%
}
\newcommand{\xleftarrow}[2][]{%
  \mathrel{\mathop{%
    \setbox\z@\vbox{\m@th
      \hbox{$\scriptstyle\;\;{#1}\;$}%
      \hbox{$\m@th\scriptstyle\;\;{#2}\;\;$}%
    }%
    \hbox to\ifdim\wd\z@>\minaw@\wd\z@\else\minaw@\fi{%
      \leftarrowfill@\displaystyle}}%
  \limits^{#2}\@ifnotempty{#1}{_{#1}}}%
}
\@ifundefined{minaw@}{\newdimen\minaw@\minaw@11pt}{}
\newcommand{\Sb}{\PackageError{amsmath}%
  {Environment `Sb' is obsolete; use `substack' instead}%
  {The \protect\\protect\ used to separate lines in a `Sb' environment can
   cause problems if `Sb' is embedded in some aligning environments.}}
\newcommand{\Sp}{\PackageError{amsmath}%
  {Environment `Sp' is obsolete; use `substack' instead}%
  {The \protect\\protect\ used to separate lines in a `Sp' environment can
   cause problems if `Sp' is embedded in some aligning environments.}}
\newenvironment{subarray}[1]{%
  \vcenter\bgroup
  \Let@ \restore@math@cr \default@tag
  \baselineskip\fontdimen10 \scriptfont\tw@
  \advance\baselineskip\fontdimen12 \scriptfont\tw@
  \lineskip\thr@@\fontdimen8 \scriptfont\thr@@
  \lineskiplimit\lineskip
  \ialign\bgroup\ifx c#1\hfil\fi
    $\m@th\scriptstyle##$\hfil\crcr
}{%
  \crcr\egroup\egroup
}
\newcommand{\substack}[1]{\subarray{c}#1\endsubarray}
\newenvironment{smallmatrix}{\null\,\vcenter\bgroup
  \Let@\restore@math@cr\default@tag
  \baselineskip6\ex@ \lineskip1.5\ex@ \lineskiplimit\lineskip
  \ialign\bgroup\hfil$\m@th\scriptstyle##$\hfil&&\thickspace\hfil
  $\m@th\scriptstyle##$\hfil\crcr
}{%
  \crcr\egroup\egroup\,%
}
\newcount\c@MaxMatrixCols \c@MaxMatrixCols=10
\renewenvironment{matrix}{%
  \hskip -\arraycolsep\array{*\c@MaxMatrixCols c}%
}{%
  \endarray \hskip -\arraycolsep
}
\renewenvironment{pmatrix}{\left(\matrix}{\endmatrix\right)}
\newenvironment{bmatrix}{\left[\matrix}{\endmatrix\right]}
\newenvironment{Bmatrix}{\left\lbrace\matrix}{\endmatrix\right\rbrace}
\newenvironment{vmatrix}{\left\lvert\matrix}{\endmatrix\right\rvert}
\newenvironment{Vmatrix}{\left\lVert\matrix}{\endmatrix\right\rVert}
\let\hdots\@ldots
\newcommand{\hdotsfor}[1]{%
  \ifx[#1\@xp\shdots@for\else\hdots@for\@ne{#1}\fi}
\newmuskip\dotsspace@
\def\shdots@for#1]{\hdots@for{#1}}
\def\hdots@for#1#2{\multicolumn{#2}c%
  {\m@th\dotsspace@1.5mu\mkern-#1\dotsspace@
   \xleaders\hbox{$\m@th\mkern#1\dotsspace@.\mkern#1\dotsspace@$}%
           \hfill
   \mkern-#1\dotsspace@}%
   }
\renewenvironment{cases}{%
  \left\{\def\arraystretch{1.2}%
  \array{@{}l@{\quad}l@{}}%
}{%
  \endarray\right.%
}
\newcounter{parentequation}% Counter for ``parent equation''.
\newenvironment{subequations}{%
  \refstepcounter{equation}%
  \begingroup % conservative approach
  \let\protect\@nx
  \edef\@tempa{\def\@nx\theparentequation{\theequation}}%
  \@xp\endgroup\@tempa
  \setcounter{parentequation}{\value{equation}}%
  \setcounter{equation}{0}%
  \def\theequation{\theparentequation\alph{equation}}%
  \ignorespaces
}{%
  \setcounter{equation}{\value{parentequation}}%
  \global\@ignoretrue
}
\def\numberwithin#1#2{\@ifundefined{c@#1}{\@nocounterr{#1}}{%
  \@ifundefined{c@#2}{\@nocnterr{#2}}{%
  \@addtoreset{#1}{#2}%
  \toks@\@xp\@xp\@xp{\csname the#1\endcsname}%
  \@xp\xdef\csname the#1\endcsname
    {\@xp\@nx\csname the#2\endcsname
     .\the\toks@}}}}
\def\eqref#1{\textup{\tagform@{\ref{#1}}}}
\newcount\dspbrk@lvl
\dspbrk@lvl=-1
\interdisplaylinepenalty\@M
\def\allowdisplaybreaks{%
  \new@ifnextchar[\allowdspbrks@{\allowdspbrks@[4]}}
\def\allowdspbrks@[#1]{%
  \interdisplaylinepenalty\getdsp@pen{#1}}
\def\getdsp@pen#1{%
  \ifcase #1\relax \@M
    \or 9999
    \or 6999
    \or 2999
    \or \z@\fi}
\def\displaybreak{\@amsmath@err{\Invalid@@\displaybreak}\@eha}

\def\displaybreak@{%
  \def\displaybreak{\new@ifnextchar[\dspbrk@{\dspbrk@[4]}}}

\def\dspbrk@[#1]{\global\dspbrk@lvl #1\relax}
\def\math@cr{{\ifnum0=`}\fi
  \@ifstar{\global\@eqpen\@M\math@cr@}%
          {\global\@eqpen
             \ifnum\dspbrk@lvl <\z@ \interdisplaylinepenalty
              \else -\@getpen\dspbrk@lvl \fi
           \math@cr@}}
\def\math@cr@{\new@ifnextchar[\math@cr@@{\math@cr@@[\z@]}}
\def\math@cr@@[#1]{\ifnum0=`{\fi}\math@cr@@@
  \noalign{\vskip#1\relax}}
\def\Let@{\let\\\math@cr}
\def\restore@math@cr{\def\math@cr@@@{\cr}}
\restore@math@cr
\def\intertext{\@amsmath@err{\Invalid@@\intertext}\@eha}
\def\intertext@{%
  \def\intertext##1{%
    \ifvmode\else\\\@empty\fi
    \noalign{%
      \penalty\postdisplaypenalty\vskip\belowdisplayskip
      \vbox{\normalbaselines
        \ifdim\linewidth=\columnwidth
        \else \parshape\@ne \@totalleftmargin \linewidth
        \fi
        \noindent##1\par}%
      \penalty\predisplaypenalty\vskip\abovedisplayskip%
    }%
}}
\newhelp\tag@help
  {tag cannot be used at this point.\space
   If you don't understand why^^Jyou should consult
   the documentation.^^JBut don't worry: just continue, and I'll
   forget what happened.}
\def\gobble@tag{\@ifstar\@gobble\@gobble}
\def\invalid@tag#1{\@amsmath@err{#1}{\the\tag@help}\gobble@tag}
\def\dft@tag{\invalid@tag{\string\tag\space not allowed here}}
\def\default@tag{\let\tag\dft@tag}
\default@tag
\def\maketag@@{\@ifstar\maketag@@@\tagform@}
\def\maketag@@@#1{\hbox{\m@th\normalfont#1}}
\def\tagform@#1{\maketag@@@{(\ignorespaces#1\unskip\@@italiccorr)}}
\iftagsleft@
  \def\@eqnnum{\hbox to1sp{}\rlap{\normalfont\normalcolor
    \hskip -\displaywidth\tagform@\theequation}}
\else
  \def\@eqnnum{{\normalfont\normalcolor \tagform@\theequation}}
\fi
\def\thetag{\leavevmode\tagform@}
\def\make@df@tag{\@ifstar\make@df@tag@@\make@df@tag@@@}
\def\make@df@tag@@#1{%
  \gdef\df@tag{\maketag@@@{#1}\def\@currentlabel{#1}}}
\def\make@df@tag@@@#1{\gdef\df@tag{\tagform@{#1}%
  \toks@\@xp{\p@equation{#1}}\edef\@currentlabel{\the\toks@}}}
\let\ltx@label\label
\def\label@in@display{%
    \ifx\df@label\@empty\else
        \@amsmath@err{Multiple \string\label's:
            label '\df@label' will be lost}\@eha
    \fi
    \gdef\df@label
}

\let\df@label\@empty
\def\make@display@tag{%
    \if@eqnsw
        \refstepcounter{equation}%
        \tagform@\theequation
    \else
        \iftag@
            \df@tag
            \global\let\df@tag\@empty
        \fi
    \fi
    \ifx\df@label\@empty\else
        \ltx@label{\df@label}%
        \global\let\df@label\@empty
    \fi
}
\def\tag@in@align{%
    \relax
    \iftag@
        \DN@{\invalid@tag{Multiple \string\tag}}%
    \else
    \global\tag@true
    \nonumber
        \let\next@\make@df@tag
    \fi
    \next@
}
\def\raisetag#1{\skip@#1\relax
  \xdef\raise@tag{\vskip\iftagsleft@\else-\fi\the\skip@\relax}%
}
\let\raise@tag\@empty
\def\notag{\nonumber}
\newif\ifinany@
\newif\ifinalign@
\newif\ifingather@
\newif\iftag@
\newif\ifst@rred
\newif\ifmeasuring@
\newif\ifshifttag@
\newcount\row@
\newcount\column@
\def\column@plus{%
    \global\advance\column@\@ne
}
\newcount\maxfields@
\def\add@amps#1{%
    \begingroup
        \count@#1
        \DN@{}%
        \loop
            \ifnum\count@>\column@
                \edef\next@{&\next@}%
                \advance\count@\m@ne
        \repeat
    \@xp\endgroup
    \next@
}
\newhelp\andhelp@
{An extra & here is so disastrous that you should probably exit^^J
and fix things up.}
\newdimen\eqnshift@
\newdimen\alignsep@
\newdimen\tagshift@
\def\mintagsep{.5\fontdimen6\textfont2}
\def\minalignsep{10pt}
\newdimen\tagwidth@
\newdimen\totwidth@
\newdimen\lineht@
\def\tag@width#1{%
    \ifcase\@xp#1\tag@lengths\fi
}

\def\savetaglength@{%
    \begingroup
        \let\or\relax
        \xdef\tag@lengths{\tag@lengths\or \the\wdz@}%
    \endgroup
}

\def\shift@tag#1{%
    \ifcase\@xp#1\tag@shifts\fi\relax
}

\let\tag@shifts\@empty
\def\saveshift@#1{%
    \begingroup
        \let\or\relax
        \xdef\tag@shifts{\or#1\tag@shifts}%
    \endgroup
}
\def\displ@y{\@display@init{}}
\def\@display@init#1{%
    \global\dt@ptrue
    \openup\jot\m@th
    \everycr{%
        \noalign{%
            #1%
            \ifdt@p
                \global\dt@pfalse
                \vskip-\lineskiplimit
                \vskip\normallineskiplimit
            \else
                \penalty\@eqpen
            \fi
        }%
    }%
}
\def\displ@y@{\@display@init{%
  \global\column@\z@ \global\dspbrk@lvl\m@ne
  \global\tag@false \global\let\raise@tag\@empty
}}
\def\black@#1{%
    \noalign{%
        \ifdim#1>\displaywidth
            \dimen@\prevdepth
            \nointerlineskip
            \vskip-\ht\strutbox@
            \vskip-\dp\strutbox@
            \vbox{\noindent\hbox to#1{\strut@\hfill}}%
            \prevdepth\dimen@
        \fi
    }%
}
\def\savecounters@{%
    \begingroup
        \def\@elt##1{%
          \global\csname c@##1\endcsname\the\csname c@##1\endcsname}%
        \xdef\@tempa{%
            \cl@@ckpt
            \let\@nx\restorecounters@\@nx\@empty
        }%
    \endgroup
    \let\restorecounters@\@tempa
}
\let\restorecounters@\@empty
\def\savealignstate@{%
    \begingroup
        \let\or\relax
        \xdef\@tempa{%
            \global\totwidth@\the\totwidth@
            \global\row@\the\row@
            \gdef\@nx\tag@lengths{\tag@lengths}%
            \let\@nx\restorealignstate@\@nx\@empty
        }%
    \endgroup
    \let\restorealignstate@\@tempa
}

\let\restorealignstate@\@empty
\newtoks\@envbody
\def\addto@envbody#1{\@envbody\@xp{\the\@envbody#1}}
\def\collect@body#1{%
    \@envbody{}%
    \def\process@envbody{%
        \@xp#1\@xp{\the\@envbody}%
    }%
    \@xp\let\csname\@currenvir\endcsname\collect@@body
    \csname\@currenvir\endcsname
}
\def\collect@@body#1\end#2{%
    \def\@tempa{#2}%
    \ifx\@tempa\@currenvir
        \addto@envbody{#1}%
        \@xp\edef\csname\@currenvir\endcsname{%
            \@nx\process@envbody\@nx\end{\@tempa}%
            }%
    \else
        \addto@envbody{#1\end{#2}}%
    \fi
    \csname\@currenvir\endcsname
}
\newcommand{\start@aligned}[2]{%
    \RIfM@\else
        \nonmatherr@{\begin{\@currenvir}}%
    \fi
    \null\,%
    \if #1t\vtop \else \if#1b \vbox \else \vcenter \fi \fi \bgroup
        \maxfields@#2\relax
        \ifnum\maxfields@>\m@ne
            \multiply\maxfields@\tw@
            \let\math@cr@@@\math@cr@@@alignedat
        \else
            \restore@math@cr
        \fi
        \Let@
        \default@tag
        \ifinany@\else\openup\jot\fi
        \column@\z@
        \ialign\bgroup
           &\column@plus
            \hfil
            \strut@
            $\m@th\displaystyle{##}$%
           &\column@plus
            $\m@th\displaystyle{{}##}$%
            \hfil
            \crcr
}
\def\math@cr@@@alignedat{%
    \ifnum\column@>\maxfields@
        \begingroup
          \measuring@false
          \@amsmath@err{Extra & on this line}%
            {\the\andhelp@}% "An extra & here is disastrous"
        \endgroup
    \fi
    \column@\z@
    \cr
}
\newenvironment{aligned}[1][c]{%
    \start@aligned{#1}\m@ne
}{%
    \crcr\egroup\egroup
}
\newcommand{\alignedat}[2][c]{%
        \start@aligned{#1}%
}
\let\endalignedat\endaligned
\newcommand{\gathered}[1][c]{%
    \RIfM@\else
        \nonmatherr@{\begin{gathered}}%
    \fi
    \null\,%
    \if #1t\vtop \else \if#1b\vbox \else \vcenter \fi\fi \bgroup
        \Let@
        \restore@math@cr
        \ifinany@\else\openup\jot\fi
        \ialign\bgroup
            \hfil\strut@$\m@th\displaystyle##$\hfil
            \crcr
}
\let\endgathered\endaligned
\def\start@gather#1{%
    \RIfM@
        \nomath@env
        \DN@{\@namedef{end\@currenvir}{}\@gobble}%
    \else
        $$%
        #1%
        \ifst@rred\else
            \global\@eqnswtrue
        \fi
        \let\next@\gather@
    \fi
    \collect@body\next@
}
\def\gather{\start@gather\st@rredfalse}

\@namedef{gather*}{\start@gather\st@rredtrue}
\def\gather@#1{%
    \ingather@true
    \inany@true
    \let\tag\tag@in@align
    \let\label\label@in@display
    \displaybreak@
    \intertext@
    \displ@y@
    \Let@
    \let\math@cr@@@\math@cr@@@gather
    \gmeasure@{#1}%
    \global\shifttag@false
    \tabskip\z@skip
    \global\row@\@ne
    \halign to\displaywidth\bgroup
        \strut@
        \setboxz@h{$\m@th\displaystyle{##}$}%
        \calc@shift@gather
        \set@gather@field
        \tabskip\@centering
       &\setboxz@h{\strut@{##}}%
        \place@tag@gather
        \tabskip \iftagsleft@ \gdisplaywidth@ \else \z@skip \span\fi
        \crcr
        #1%
}
\def\endgather{%
        \math@cr
        \black@\totwidth@
    \egroup
    $$%
    \global\@ignoretrue
}

\@xp\let\csname endgather*\endcsname\endgather
\def\gmeasure@#1{%
    \begingroup
        \measuring@true
        \totwidth@\z@
        \global\let\tag@lengths\@empty
        \savecounters@
        \setbox\@ne\vbox{%
            \everycr{\noalign{\global\tag@false
              \global\let\raise@tag\@empty \global\column@\z@}}%
            \let\label\@gobble
            \halign{%
                \setboxz@h{$\m@th\displaystyle{##}$}%
                \ifdim\wdz@>\totwidth@
                    \global\totwidth@\wdz@
                \fi
               &\setboxz@h{\strut@{##}}%
                \savetaglength@
                \crcr
                #1%
                \math@cr@@@
            }%
        }%
        \restorecounters@
        \if@fleqn
            \global\advance\totwidth@\@mathmargin
        \fi
        \iftagsleft@
            \ifdim\totwidth@>\displaywidth
                \global\let\gdisplaywidth@\totwidth@
            \else
                \global\let\gdisplaywidth@\displaywidth
            \fi
        \fi
    \endgroup
}
\def\math@cr@@@gather{%
    \ifst@rred\nonumber\fi
   &\relax
    \make@display@tag
    \ifst@rred\else\global\@eqnswtrue\fi
    \global\advance\row@\@ne
    \cr
}
\def\calc@shift@gather{%
    \dimen@\mintagsep\relax
    \tagwidth@\tag@width\row@\relax
    \if@fleqn
        \global\eqnshift@\@mathmargin
        \ifdim\tagwidth@>\z@
            \advance\dimen@\tagwidth@
            \iftagsleft@
                \ifdim\dimen@>\@mathmargin
                    \global\shifttag@true
                \fi
            \else
                \advance\dimen@\@mathmargin
                \advance\dimen@\wdz@
                \ifdim\dimen@>\displaywidth
                   \global\shifttag@true
                \fi
            \fi
        \fi
    \else
        \global\eqnshift@\displaywidth
        \global\advance\eqnshift@-\wdz@
        \ifdim\tagwidth@>\z@
            \multiply\dimen@\tw@
            \advance\dimen@\wdz@
            \advance\dimen@\tagwidth@
            \ifdim\dimen@>\displaywidth
                \global\shifttag@true
            \else
                \ifdim\eqnshift@<4\tagwidth@
                    \global\advance\eqnshift@-\tagwidth@
                \fi
            \fi
        \fi
        \global\divide\eqnshift@\tw@
        \iftagsleft@
            \global\eqnshift@-\eqnshift@
            \global\advance\eqnshift@\displaywidth
            \global\advance\eqnshift@-\wdz@
        \fi
        \ifdim\eqnshift@<\z@
            \global\eqnshift@\z@
        \fi
    \fi
}
\def\place@tag@gather{%
    \iftagsleft@
        \kern-\gdisplaywidth@
        \ifshifttag@
            \rlap{\vbox{%
                \normalbaselines
                \boxz@
                \vbox to\lineht@{}%
                \raise@tag
            }}%
            \global\shifttag@false
        \else
            \rlap{\boxz@}%
        \fi
    \else
        \ifdim\totwidth@>\displaywidth
            \dimen@\totwidth@
            \advance\dimen@-\displaywidth
            \kern-\dimen@
        \fi
        \ifshifttag@
            \llap{\vtop{%
                \raise@tag
                \normalbaselines
                \setbox\@ne\null
                \dp\@ne\lineht@
                \box\@ne
                \boxz@
            }}%
            \global\shifttag@false
        \else
            \llap{\boxz@}%
        \fi
    \fi
}
\def\set@gather@field{%
    \iftagsleft@
        \global\lineht@\ht\z@
    \else
        \global\lineht@\dp\z@
    \fi
    \kern\eqnshift@
    \boxz@
    \hfil
}
\newif\ifxxat@

\newif\ifcheckat@

\let\xatlevel@\@empty
\def\start@align#1#2#3{%
    \let\xatlevel@#1% always \z@, \@ne, or \tw@
    \maxfields@#3\relax
    \ifnum\maxfields@>\m@ne
        \checkat@true
        \ifnum\xatlevel@=\tw@
            \xxat@true
        \fi
        \multiply\maxfields@\tw@
    \else
        \checkat@false
    \fi
    \ifingather@
        {\ifnum0=`}\fi
        \DN@{\vcenter\bgroup\savealignstate@\align@#2}%
    \else
        \ifmmode
            \nomath@env
            \DN@{\@namedef{end\@currenvir}{}\@gobble}%
        \else
            $$%
            \DN@{\align@#2}%
        \fi
    \fi
    \collect@body\next@
}
\def\alignat{\start@align\z@\st@rredfalse}
\@namedef{alignat*}{\start@align\z@\st@rredtrue}
\def\xalignat{\start@align\@ne\st@rredfalse}
\@namedef{xalignat*}{\start@align\@ne\st@rredtrue}
\def\xxalignat{\start@align\tw@\st@rredtrue}
\def\align{\start@align\@ne\st@rredfalse\m@ne}
\@namedef{align*}{\start@align\@ne\st@rredtrue\m@ne}
\def\flalign{\start@align\tw@\st@rredfalse\m@ne}
\@namedef{flalign*}{\start@align\tw@\st@rredtrue\m@ne}
\def\align@#1#2{%
    \inany@true
    \inalign@true
    \displaybreak@
    \intertext@
    \ifingather@\else\displ@y@\fi
    \Let@
    \let\math@cr@@@\math@cr@@@align
    \ifxxat@\else
        \let\tag\tag@in@align
    \fi
    \let\label\label@in@display
    #1% set st@r
    \ifst@rred\else
        \global\@eqnswtrue
    \fi
    \measure@{#2}%
    \global\row@\z@
    \tabskip\eqnshift@
    \halign\bgroup
        \span\align@preamble\crcr
        #2%
}
\def\endalign{%
        \math@cr
        \black@\totwidth@
    \egroup
    \ifingather@
        \restorealignstate@
        \egroup
        \nonumber
        \ifnum0=`{\fi}%
    \else
        $$%
    \fi
    \global\@ignoretrue
}
\@xp\let\csname endalign*\endcsname\endalign
\let\endxalignat\endalign
\@xp\let\csname endxalignat*\endcsname\endalign
\let\endxxalignat\endalign
\let\endalignat\endalign
\@xp\let\csname endalignat*\endcsname\endalign
\let\endflalign\endalign
\@xp\let\csname endflalign*\endcsname\endalign
\def\math@cr@@@align{%
    \kern-\alignsep@
    \ifst@rred\nonumber\fi
    \if@eqnsw \global\tag@true \fi
    \global\advance\row@\@ne
    \iftag@
        \add@amps\maxfields@
        \omit
        \setboxz@h{\@lign\strut@{\make@display@tag}}%
        \place@tag
    \fi
    \ifst@rred\else\global\@eqnswtrue\fi
    \global\lineht@\z@
    \cr
}
\def\math@cr@@@align@measure{%
   &\omit
    \global\advance\row@\@ne
    \ifst@rred\nonumber\fi
    \if@eqnsw \global\tag@true \fi
    \ifnum\column@>\maxfields@
        \ifcheckat@
            \begingroup
              \measuring@false
              \@amsmath@err{Extra & on this line}%
                {\the\andhelp@}% "An extra & here is disastrous"
            \endgroup
        \else
            \global\maxfields@\column@
        \fi
    \fi
    \setboxz@h{\@lign\strut@{%
        \if@eqnsw
            \stepcounter{equation}%
            \tagform@\theequation
        \else
            \iftag@\df@tag\fi
        \fi
    }}%
    \savetaglength@
    \ifst@rred\else\global\@eqnswtrue\fi
    \cr
}
\let\field@lengths\@empty

\def\savefieldlength@{%
    \begingroup
        \let\or\relax
        \xdef\field@lengths{%
            \field@lengths
            \ifnum\column@=0
                \or
            \else
                ,%
            \fi
            \the\wdz@
        }%
    \endgroup
}

\def\fieldlengths@#1{%
    \ifcase\@xp#1\field@lengths\fi
}
\let\maxcolumn@widths\@empty
\def\maxcol@width#1{%
    \ifcase\@xp#1\maxcolumn@widths\fi\relax
}
\def\measure@#1{%
    \begingroup
        \measuring@true
        \eqnshift@\z@
        \alignsep@\z@
        \global\let\tag@lengths\@empty
        \global\let\field@lengths\@empty
        \savecounters@
        \global\setbox0\vbox{%
            \let\math@cr@@@\math@cr@@@align@measure
            \everycr{\noalign{\global\tag@false
              \global\let\raise@tag\@empty \global\column@\z@}}%
            \let\label\@gobble
            \global\row@\z@
            \tabskip\z@
            \halign{\span\align@preamble\crcr
                #1%
                \math@cr@@@
                \column@\z@
                \add@amps\maxfields@\cr
            }%
        }%
        \restorecounters@
        \ifodd\maxfields@
            \global\advance\maxfields@\@ne
        \fi
        \ifnum\xatlevel@=\tw@
            \ifnum\maxfields@<\thr@@
                \let\xatlevel@\z@
            \fi
        \fi
        \setbox0\vbox{%
            \unvbox0
            \unpenalty
            \global\setbox1\lastbox
        }%
        \global\totwidth@\wd1
        \if@fleqn
           \global\advance\totwidth@\@mathmargin
        \fi
        \global\let\maxcolumn@widths\@empty
        \begingroup
            \let\or\relax
            \loop
               \setbox1\hbox{%
                   \unhbox1
                   \unskip
                   \global\setbox0\lastbox
               }%
                \ifhbox0
                    \xdef\maxcolumn@widths{ \or \the\wd0 \maxcolumn@widths}%
            \repeat
        \endgroup
        \dimen@\displaywidth
        \advance\dimen@-\totwidth@
        \ifcase\xatlevel@
            \global\alignsep@\z@
            \let\minalignsep\z@
            \@tempcntb\z@
            \if@fleqn
                \@tempcnta\@ne
                \global\eqnshift@\@mathmargin
            \else
                \@tempcnta\tw@
                \global\eqnshift@\dimen@
                \global\divide\eqnshift@\@tempcnta
            \fi
        \or
            \@tempcntb\maxfields@
            \divide\@tempcntb\tw@
            \@tempcnta\@tempcntb
            \advance\@tempcntb\m@ne
            \if@fleqn
                \global\eqnshift@\@mathmargin
                \alignsep@\dimen@
                \global\divide\alignsep@\@tempcnta
            \else
                \global\advance\@tempcnta\@ne
                \global\eqnshift@\dimen@
                \global\divide\eqnshift@\@tempcnta
                \global\alignsep@\eqnshift@
            \fi
        \or
            \@tempcntb\maxfields@
            \divide\@tempcntb\tw@
            \global\advance\@tempcntb\m@ne
            \global\@tempcnta\@tempcntb
            \global\eqnshift@\z@
            \global\alignsep@\dimen@
            \if@fleqn
                \advance\alignsep@\@mathmargin\relax
            \fi
            \global\divide\alignsep@\@tempcntb
        \fi
        \ifdim\alignsep@<\minalignsep\relax
            \global\alignsep@\minalignsep\relax
            \ifdim\eqnshift@>\z@
                \if@fleqn\else
                    \eqnshift@\displaywidth
                    \advance\eqnshift@-\totwidth@
                    \advance\eqnshift@-\@tempcntb\alignsep@
                    \global\divide\eqnshift@\tw@
                \fi
            \fi
        \fi
        \ifdim\eqnshift@<\z@
            \global\eqnshift@\z@
        \fi
        \calc@shift@align
        \tagshift@\totwidth@
        \advance\tagshift@\@tempcntb\alignsep@
        \if@fleqn
            \ifnum\xatlevel@=\tw@
                \global\advance\tagshift@-\@mathmargin\relax
            \fi
        \else
            \global\advance\tagshift@\eqnshift@
        \fi
        \iftagsleft@ \else
            \global\advance\tagshift@-\displaywidth
        \fi
        \dimen@\minalignsep\relax
        \advance\totwidth@\@tempcntb\dimen@
        \ifdim\totwidth@>\displaywidth
            \global\let\displaywidth@\totwidth@
        \else
            \global\let\displaywidth@\displaywidth
        \fi
    \endgroup
}
\iftagsleft@\if@fleqn
    \def\calc@shift@align{%
        \global\let\tag@shifts\@empty
        \begingroup
            \@tempdima\@mathmargin\relax
            \advance\@tempdima-\mintagsep\relax
            \loop
                \ifnum\row@>0
                    \ifdim\tag@width\row@>\z@
                        \x@calc@shift@lf
                    \else
                        \saveshift@0%
                    \fi
                    \advance\row@\m@ne
            \repeat
        \endgroup
    }
    \def\x@calc@shift@lf{%
        \ifdim\eqnshift@=\z@
            \global\eqnshift@\@mathmargin\relax
            \alignsep@\displaywidth
            \advance\alignsep@-\totwidth@
            \global\divide\alignsep@\@tempcntb
            \ifdim\alignsep@<\minalignsep\relax
                \global\alignsep@\minalignsep\relax
            \fi
        \fi
        \ifdim\tag@width\row@>\@tempdima
            \saveshift@1%
        \else
            \saveshift@0%
        \fi
    }
\fi\fi
\iftagsleft@\else\if@fleqn
    \def\calc@shift@align{%
        \global\let\tag@shifts\@empty
        \begingroup
            \loop
                \ifnum\row@>0
                    \ifdim\tag@width\row@>\z@
                        \x@calc@shift@rf
                    \else
                        \saveshift@0%
                    \fi
                    \advance\row@\m@ne
            \repeat
        \endgroup
    }
    \def\x@calc@shift@rf{%
        \column@\z@
        \@tempdimb\z@
        \@tempdimc\z@
        \edef\@tempb{\fieldlengths@\row@}%
        \@for\@tempa:=\@tempb\do{%
            \advance\column@\@ne
            \x@rcalc@width
        }%
        \begingroup
            \advance\column@\m@ne
            \divide\column@\tw@
            \ifnum\@tempcntb>\column@
                \advance\@tempcnta-\@tempcntb
                \advance\@tempcnta\column@
                \@tempcntb\column@
            \fi
            \tagwidth@\tag@width\row@\relax
            \@tempdima\eqnshift@
            \advance\@tempdima\@tempdimc\relax
            \advance\@tempdima\tagwidth@
            \dimen@\minalignsep\relax
            \multiply\dimen@\@tempcntb
            \advance\dimen@\mintagsep\relax
            \advance\dimen@\@tempdima
            \ifdim\dimen@>\displaywidth
                \saveshift@1%
            \else
                \saveshift@0%
                \dimen@\alignsep@\relax
                \multiply\dimen@\@tempcntb
                \advance\dimen@\@tempdima
                \advance\dimen@\tagwidth@
                \ifdim\dimen@>\displaywidth
                    \dimen@\displaywidth
                    \advance\dimen@-\@tempdima
                    \ifnum\xatlevel@=\tw@
                        \advance\dimen@-\mintagsep\relax
                    \fi
                    \divide\dimen@\@tempcnta
                    \ifdim\dimen@<\minalignsep\relax
                        \global\alignsep@\minalignsep\relax
                    \else
                        \global\alignsep@\dimen@
                    \fi
                \fi
            \fi
        \endgroup
    }
\fi\fi
\iftagsleft@\else\if@fleqn\else
    \def\calc@shift@align{%
        \global\let\tag@shifts\@empty
        \begingroup
            \loop
                \ifnum\row@>0
                    \ifdim\tag@width\row@>\z@
                        \x@calc@shift@rc
                    \else
                        \saveshift@0%
                    \fi
                    \advance\row@\m@ne
            \repeat
        \endgroup
    }
    \def\x@calc@shift@rc{%
        \column@\z@
        \@tempdimb\z@
        \@tempdimc\z@
        \edef\@tempb{\fieldlengths@\row@}%
        \@for\@tempa:=\@tempb\do{%
            \advance\column@\@ne
            \x@rcalc@width
        }%
        \begingroup
            \advance\column@\m@ne
            \divide\column@\tw@
            \ifnum\@tempcntb>\column@
                \advance\@tempcnta-\@tempcntb
                \advance\@tempcnta\column@
                \@tempcntb\column@
            \fi
            \tagwidth@\tag@width\row@\relax
            \@tempdima\@tempdimc
            \advance\@tempdima\tagwidth@
            \dimen@\minalignsep\relax
            \multiply\dimen@\@tempcntb
            \advance\dimen@\mintagsep\relax
            \ifnum\xatlevel@=\tw@ \else
                \advance\dimen@\mintagsep\relax
            \fi
            \advance\dimen@\@tempdima
            \ifdim\dimen@>\displaywidth
                \saveshift@1%
            \else
                \saveshift@0%
                \dimen@\eqnshift@
                \advance\dimen@\@tempdima
                \advance\dimen@\@tempcntb\alignsep@
                \advance\dimen@\tagwidth@
                \ifdim\dimen@>\displaywidth
                    \dimen@\displaywidth
                    \advance\dimen@-\@tempdima
                    \ifnum\xatlevel@=\tw@
                        \advance\dimen@-\mintagsep\relax
                    \fi
                    \divide\dimen@\@tempcnta
                    \ifdim\dimen@<\minalignsep\relax
                        \global\alignsep@\minalignsep\relax
                        \eqnshift@\displaywidth
                        \advance\eqnshift@-\@tempdima
                        \advance\eqnshift@-\@tempcntb\alignsep@
                        \global\divide\eqnshift@\tw@
                    \else
                        \ifdim\dimen@<\eqnshift@
                            \ifdim\dimen@<\z@
                                \global\eqnshift@\z@
                            \else
                                \global\eqnshift@\dimen@
                            \fi
                        \fi
                        \ifdim\dimen@<\alignsep@
                            \global\alignsep@\dimen@
                        \fi
                    \fi
                \fi
            \fi
        \endgroup
    }
\fi\fi
\iftagsleft@\else
    \def\x@rcalc@width{%
        \ifdim\@tempa > \z@
            \advance\@tempdimc\@tempdimb
            \ifodd\column@
                \advance\@tempdimc\maxcol@width\column@
                \@tempdimb\z@
            \else
                \advance\@tempdimc\@tempa\relax
                \@tempdimb\maxcol@width\column@
                \advance\@tempdimb-\@tempa\relax
            \fi
        \else
            \advance\@tempdimb\maxcol@width\column@\relax
        \fi
    }
\fi
\iftagsleft@\if@fleqn\else
    \def\calc@shift@align{%
        \global\let\tag@shifts\@empty
        \begingroup
            \loop
                \ifnum\row@>\z@
                    \ifdim\tag@width\row@>\z@
                        \x@calc@shift@lc
                    \else
                        \saveshift@0%
                    \fi
                    \advance\row@\m@ne
            \repeat
        \endgroup
    }
    \def\x@calc@shift@lc{%
        \column@\z@
        \@tempdima\z@ % ``width of equation''
        \@tempdimb\z@ % ``indent of equation''
        \edef\@tempb{\fieldlengths@\row@}%
        \@for\@tempa:=\@tempb\do{%
            \advance\column@\@ne
            \x@lcalc@width
        }%
        \begingroup
            \tagwidth@\tag@width\row@\relax
            \@tempdima\totwidth@
            \advance\@tempdima-\@tempdimb
            \advance\@tempdima\tagwidth@
            \dimen@\minalignsep\relax
            \multiply\dimen@\@tempcntb
            \advance\dimen@\mintagsep\relax
            \ifnum\xatlevel@=\tw@ \else
                \advance\dimen@\mintagsep\relax
            \fi
            \advance\dimen@\@tempdima
            \ifdim\dimen@>\displaywidth
                \saveshift@1%
            \else
                \saveshift@0%
                \dimen@\alignsep@
                \multiply\dimen@\count@
                \advance\dimen@\eqnshift@
                \advance\dimen@\@tempdimb
                \ifdim\dimen@<2\tagwidth@
                    \dimen@\displaywidth
                    \advance\dimen@-\@tempdima
                    \ifnum\xatlevel@=\tw@
                        \advance\dimen@-\mintagsep\relax
                    \fi
                    \divide\dimen@\@tempcnta
                    \ifdim\dimen@<\minalignsep\relax
                        \global\alignsep@\minalignsep\relax
                        \dimen@\displaywidth
                        \advance\dimen@-\@tempdima
                        \advance\dimen@-\@tempcntb\alignsep@
                        \global\divide\dimen@\tw@
                    \else
                        \ifdim\dimen@<\alignsep@
                            \global\alignsep@\dimen@
                        \fi
                    \fi
                    \ifnum\xatlevel@=\tw@
                        \dimen@\mintagsep\relax
                    \fi
                    \advance\dimen@\tagwidth@
                    \advance\dimen@-\@tempdimb
                    \advance\dimen@-\count@\alignsep@
                    \ifdim\dimen@>\eqnshift@
                        \global\eqnshift@\dimen@
                    \fi
                \fi
            \fi
        \endgroup
    }
    \def\x@lcalc@width{%
        \ifdim\@tempdima = \z@
            \ifdim\@tempa > \z@
                \@tempdima\p@
                \ifodd\column@
                    \advance\@tempdimb \maxcol@width\column@
                    \advance\@tempdimb-\@tempa
                \fi
                \count@\column@
                \advance\count@\m@ne
                \divide\count@\tw@
                \advance\@tempcnta-\count@
                \advance\@tempcntb-\count@
            \else
                \advance\@tempdimb \maxcol@width\column@\relax
            \fi
        \fi
    }
\fi\fi
\def\place@tag{%
    \iftagsleft@
        \kern-\tagshift@
        \if1\shift@tag\row@\relax
            \rlap{\vbox{%
                \normalbaselines
                \boxz@
                \vbox to\lineht@{}%
                \raise@tag
            }}%
        \else
            \rlap{\boxz@}%
        \fi
        \kern\displaywidth@
    \else
        \kern-\tagshift@
        \if1\shift@tag\row@\relax
            \llap{\vtop{%
                \raise@tag
                \normalbaselines
                \setbox\@ne\null
                \dp\@ne\lineht@
                \box\@ne
                \boxz@
            }}%
        \else
            \llap{\boxz@}%
        \fi
    \fi
}
\def\align@preamble{%
   &\hfil
    \strut@
    \setboxz@h{\@lign$\m@th\displaystyle{##}$}%
    \ifmeasuring@\savefieldlength@\fi
    \set@field
    \tabskip\z@skip
   &\setboxz@h{\@lign$\m@th\displaystyle{{}##}$}%
    \ifmeasuring@\savefieldlength@\fi
    \set@field
    \hfil
    \tabskip\alignsep@
}
\def\set@field{%
    \column@plus
    \iftagsleft@
        \ifdim\ht\z@>\lineht@
            \global\lineht@\ht\z@
        \fi
    \else
        \ifdim\dp\z@>\lineht@
            \global\lineht@\dp\z@
        \fi
    \fi
    \boxz@
}
\def\split{%
    \ifinany@
        \@xp\insplit@
    \else
        \@xp\split@err
    \fi
}
\edef\split@err{%
    \@nx\@amsmath@err{%
        \string\begin{split} won't work here%
    }{%
        \@xp\@nx\csname
  Did you forget a preceding \string\begin{equation}?^^J%
  If not, perhaps the `aligned' environment is what you want.\endcsname}%
}
\def\insplit@{%
    \global\setbox\z@\vbox\bgroup
        \Let@
        \restore@math@cr
        \default@tag % disallow use of \tag here
        \ialign\bgroup
            \hfil
            \strut@
            $\m@th\displaystyle{##}$%
           &$\m@th\displaystyle{{}##}$%
            \hfill % Why not \hfil?---dmj, 1994/12/28
            \crcr
}
\def\endsplit{%
            \crcr
        \egroup
    \egroup
    \iftagsleft@
        \@xp\lendsplit@
    \else
        \@xp\rendsplit@
    \fi
}
\def\rendsplit@{%
    \ifinalign@
        \global\setbox9 \vtop{%
            \unvcopy\z@
            \global\setbox8 \lastbox
            \unskip
        }%
        \setbox\@ne\hbox{%
            \unhcopy8
            \unskip
            \global\setbox\tw@\lastbox
            \unskip
            \global\setbox\thr@@\lastbox
        }%
        \ifctagsplit@
            \gdef\split@{%
                \hbox to\wd\thr@@{}%
               &\vcenter{\vbox{\moveleft\wd\thr@@\boxz@}}%
            }%
        \else
            \global\setbox7 \hbox{\unhbox\tw@\unskip}%
            \gdef\split@{%
                \global\@tempcnta\column@
               &\setboxz@h{}%
                \savetaglength@
                \global\advance\row@\@ne
                \vbox{\moveleft\wd\thr@@\box9}%
                \crcr
                \noalign{\global\lineht@\z@}%
                \add@amps\@tempcnta
                \box\thr@@
               &\box7
            }%
        \fi
    \else
        \ifctagsplit@
            \gdef\split@{\vcenter{\boxz@}}%
        \else
            \gdef\split@{%
                \boxz@
            }%
        \fi
    \fi
    \aftergroup\split@
}
\def\lendsplit@{%
    \global\setbox9\vtop{\unvcopy\z@}%
    \ifinalign@
        \setbox\@ne\vbox{%
            \unvcopy\z@
            \global\setbox8\lastbox
        }%
        \setbox\@ne\hbox{%
            \unhcopy8%
            \unskip
            \setbox\tw@\lastbox
            \unskip
            \global\setbox\thr@@\lastbox
        }%
        \ifctagsplit@
            \gdef\split@{%
                \hbox to\wd\thr@@{}%
               &\vcenter{\vbox{\moveleft\wd\thr@@\box9}}%
            }%
        \else
            \gdef\split@{%
                \hbox to\wd\thr@@{}%
               &\vbox{\moveleft\wd\thr@@\box9}%
            }%
        \fi
    \else
        \ifctagsplit@
            \gdef\split@{\vcenter{\box9}}%
        \else
            \gdef\split@{\box9}%
        \fi
    \fi
    \aftergroup\split@
}
\newskip\multlinegap
\multlinegap10pt
\newskip\multlinetaggap
\multlinetaggap10pt
\def\start@multline#1{%
    \RIfM@
        \nomath@env
        \DN@{\@namedef{end\@currenvir}{}\@gobble}%
    \else
        $$%
        #1%
        \ifst@rred
            \nonumber
        \else
            \global\@eqnswtrue
        \fi
        \let\next@\multline@
    \fi
    \collect@body\next@
}
\def\multline{\start@multline\st@rredfalse}
\@namedef{multline*}{\start@multline\st@rredtrue}
\def\multline@#1{%
    \inany@true
    \Let@
    \@display@init{\global\advance\row@\@ne \global\dspbrk@lvl\m@ne}%
    \displaybreak@
    \restore@math@cr
    \let\tag\tag@in@align
    \global\tag@false \global\let\raise@tag\@empty
    \mmeasure@{#1}%
    \let\tag\gobble@tag \let\label\@gobble
    \tabskip \if@fleqn \@mathmargin \else \z@skip \fi
    \totwidth@\displaywidth
    \if@fleqn
        \advance\totwidth@-\@mathmargin
    \fi
    \halign\bgroup
        \hbox to\totwidth@{%
            \if@fleqn
                \hskip \@centering \relax
            \else
                \hfil
            \fi
            \strut@
            $\m@th\displaystyle{}##$%
            \hfil
        }%
        \crcr
        \if@fleqn
            \hskip-\@mathmargin
        \else
            \hfilneg
        \fi
        \iftagsleft@
            \iftag@
                \begingroup
                    \ifshifttag@
                        \rlap{\vbox{%
                                \normalbaselines
                                \hbox{%
                                    \strut@
                                    \make@display@tag
                                }%
                                \vbox to\lineht@{}%
                                \raise@tag
                        }}%
                        \hskip\multlinegap
                    \else
                        \make@display@tag
                        \hskip\multlinetaggap
                    \fi
                \endgroup
            \else
                \hskip\multlinegap
            \fi
        \else
            \hskip\multlinegap
        \fi
    #1%
}
\def\endmultline{%
    \iftagsleft@
        \@xp\lendmultline@
    \else
        \@xp\rendmultline@
    \fi
    \global\@ignoretrue
}
\@xp\let\csname endmultline*\endcsname=\endmultline
\def\lendmultline@{%
        \hfilneg
        \hskip\multlinegap
        \math@cr
    \egroup
    $$%
}
\def\rendmultline@{%
    \iftag@
        \begingroup
            \ifshifttag@
                \hskip\multlinegap
                \llap{\vtop{%
                    \raise@tag
                    \normalbaselines
                    \setbox\@ne\null
                    \dp\@ne\lineht@
                    \box\@ne
                    \hbox{\strut@\make@display@tag}%
                }}%
            \else
                \hskip\multlinetaggap
                \make@display@tag
            \fi
        \endgroup
    \else
        \hskip\multlinegap
    \fi
    \hfilneg
        \math@cr
    \egroup$$%
}
\def\mmeasure@#1{%
    \begingroup
        \measuring@true
        \def\label##1{%
          \begingroup\measuring@false\label@in@display{##1}\endgroup}%
        \def\math@cr@@@{\cr}%
        \let\shoveleft\@iden \let\shoveright\@iden
        \savecounters@
        \global\row@\z@
        \setbox\@ne\vbox{%
            \global\let\df@tag\@empty
            \halign{%
                \setboxz@h{\@lign$\m@th\displaystyle{}##$}%
                \iftagsleft@
                    \ifnum\row@=\@ne
                        \global\totwidth@\wdz@
                        \global\lineht@\ht\z@
                    \fi
                \else
                    \global\totwidth@\wdz@
                    \global\lineht@\dp\z@
                \fi
                \crcr
                #1%
                \crcr
            }%
        }%
        \ifx\df@tag\@empty\else\global\tag@true\fi
        \if@eqnsw\global\tag@true\fi
        \iftag@
            \setboxz@h{%
                \if@eqnsw
                    \stepcounter{equation}%
                    \tagform@\theequation
                \else
                    \df@tag
                \fi
            }%
            \global\tagwidth@\wdz@
            \dimen@\totwidth@
            \advance\dimen@\tagwidth@
            \advance\dimen@\multlinetaggap
            \iftagsleft@\else
                \if@fleqn
                    \advance\dimen@\@mathmargin
                \fi
            \fi
            \ifdim\dimen@>\displaywidth
                \global\shifttag@true
            \else
                \global\shifttag@false
            \fi
        \fi
        \restorecounters@
    \endgroup
}
\iftagsleft@
    \def\shoveright#1{%
        #1%
        \hfilneg
        \hskip\multlinegap
    }
\else
    \def\shoveright#1{%
        #1%
        \hfilneg
        \iftag@
            \ifshifttag@
                \hskip\multlinegap
            \else
                \hskip\tagwidth@
                \hskip\multlinetaggap
            \fi
        \else
            \hskip\multlinegap
        \fi
    }
\fi

\if@fleqn
    \def\shoveleft#1{#1}%
\else
    \iftagsleft@
        \def\shoveleft#1{%
            \setboxz@h{$\m@th\displaystyle{}#1$}%
            \setbox\@ne\hbox{$\m@th\displaystyle#1$}%
            \hfilneg
            \iftag@
                \ifshifttag@
                    \hskip\multlinegap
                \else
                    \hskip\tagwidth@
                    \hskip\multlinetaggap
                \fi
            \else
                \hskip\multlinegap
            \fi
            \hskip.5\wd\@ne
            \hskip-.5\wdz@
            #1%
        }
    \else
        \def\shoveleft#1{%
            \setboxz@h{$\m@th\displaystyle{}#1$}%
            \setbox\@ne\hbox{$\m@th\displaystyle#1$}%
            \hfilneg
            \hskip\multlinegap
            \hskip.5\wd\@ne
            \hskip-.5\wdz@
            #1%
        }
    \fi
\fi
\def\[{%
    \RIfM@
        \@badmath
    \else
        \DN@{%
            $$%
            \ingather@true
            \inany@true
            \def\\{\@amsmath@err{\Invalid@@\\}\@eha}%
            \tabskip\@mathmargin
            \halign to\displaywidth\bgroup
                \if@fleqn\else\hfil\fi
                \setboxz@h{$\m@th\displaystyle{####}$}%
                \global\totwidth@\wdz@
                \boxz@
                \hfil
                \tabskip\@centering
                \cr
        }%
        \@xp\next@
    \fi
}

\def\]{%
    \RIfM@
        \DN@{%
                \crcr
            \black@\totwidth@
            \egroup
            $$%
        }%
        \@xp\next@
    \else
        \@badmath
    \fi
}
\@xp\def\@xp\@arrayparboxrestore\@xp{\@arrayparboxrestore
  \inany@false\ingather@false\inalign@false \default@tag}
\def\equation{\gather\def\\{\@amsmath@err{\Invalid@@\\}\@eha}}
\def\endequation{\endgather}
\newenvironment{equation*}{%
  \equation
}{%
  \nonumber\endequation
}
%%%%%%%%%%%%%%%%% end of amsmath.sty code

\newcommand{\uppercasenonmath}[1]{\toks@\@emptytoks
  \@xp\@skipmath\@xp\@empty#1$$%
  \edef#1{\@nx\@upprep\the\toks@}%
}
\newcommand{\@upprep}{%
  \spaceskip1.3\fontdimen2\font plus1.3\fontdimen3\font
  \upchars@}
\newcommand{\upchars@}{%
  \def\ss{SS}\def\i{I}\def\j{J}\def\ae{\AE}\def\oe{\OE}%
  \def\o{\O}\def\aa{\AA}\def\l{\L}\def\Mc{M{\scshape c}}}
\newcommand{\@skipmath}{}
\long\def\@skipmath#1$#2${%
  \@xskipmath#1\(\)%
  \@ifnotempty{#2}{\toks@\@xp{\the\toks@$#2$}\@skipmath\@empty}}%
\newcommand{\@xskipmath}{}
\long\def\@xskipmath#1\(#2\){%
  \uppercase{\toks@\@xp\@xp\@xp{\@xp\the\@xp\toks@#1}}%
  \@ifnotempty{#2}{\toks@\@xp{\the\toks@\(#2\)}\@xskipmath\@empty}}%
\newcommand{\today}{%
  \relax\ifcase\month\or
  January\or February\or March\or April\or May\or June\or
  July\or August\or September\or October\or November\or December\fi
  \space\number\day, \number\year}
\DeclareOldFontCommand{\rm}{\normalfont\rmfamily}{\mathrm}
\DeclareOldFontCommand{\sf}{\normalfont\sffamily}{\mathsf}
\DeclareOldFontCommand{\tt}{\normalfont\ttfamily}{\mathtt}
\DeclareOldFontCommand{\bf}{\normalfont\bfseries}{\mathbf}
\DeclareOldFontCommand{\it}{\normalfont\itshape}{\mathit}
\DeclareOldFontCommand{\sl}{\normalfont\slshape}{\@nomath\sl}
\DeclareOldFontCommand{\sc}{\normalfont\scshape}{\@nomath\sc}
\renewcommand*{\title}[2][]{\gdef\shorttitle{#1}\gdef\@title{#2}}
\edef\title{\@nx\@dblarg
  \@xp\@nx\csname\string\title\endcsname}
\renewcommand{\author}[2][]{%
  \ifx\@empty\authors
    \gdef\shortauthors{#1}\gdef\authors{#2}%
  \else
    \g@addto@macro\shortauthors{\and#1}%
    \g@addto@macro\authors{\and#2}%
    \g@addto@macro\addresses{\author{}}%
  \fi
}
\edef\author{\@nx\@dblarg
  \@xp\@nx\csname\string\author\endcsname}
\let\shortauthors\@empty   \let\authors\@empty
\let\addresses\@empty      \let\thankses\@empty
\newcommand{\address}[2][]{\g@addto@macro\addresses{\address{#1}{#2}}}
\newcommand{\curraddr}[2][]{\g@addto@macro\addresses{\curraddr{#1}{#2}}}
\newcommand{\email}[2][]{\g@addto@macro\addresses{\email{#1}{#2}}}
\newcommand{\urladdr}[2][]{\g@addto@macro\addresses{\urladdr{#1}{#2}}}
\renewcommand{\thanks}[1]{\g@addto@macro\thankses{\thanks{#1}}}
\def\enddoc@text{\ifx\@empty\@translators \else\@settranslators\fi
  \ifx\@empty\addresses \else\@setaddresses\fi}
\AtEndDocument{\enddoc@text}
\let\@date\@empty
\def\dedicatory#1{\def\@dedicatory{#1}}
\let\@dedicatory=\@empty
\def\keywords#1{\def\@keywords{#1}}
\let\@keywords=\@empty
\def\twokname{2000}
\newcommand\subjclass[2][]{%                 8/2000 ynm
\def\testname{#1}\ifx\testname\twokname%
\def\subjclassname{\textup{2000} Mathematics Subject 
Classification}\fi\def\@subjclass{#2}}  % subj
\let\@subjclass=\@empty
\def\commby#1{\def\@commby{(Communicated by #1)}}
\let\@commby=\@empty
\def\translator#1{%
  \ifx\@empty\@translators \def\@translators{#1}%
  \else\g@addto@macro\@translators{\and#1}\fi}
\let\@translators=\@empty
\def\@settranslators{\par\begingroup
  \addvspace{6\p@\@plus9\p@}%
  \hbox to\columnwidth{\hss\normalfont\normalsize
    Translated by %
    \andify\@translators \uppercasenonmath\@translators
    \@translators}
  \endgroup
}
\newcommand{\xandlist}[4]{\@andlista{{#1}{#2}{#3}}#4\and\and}
\def\@andlista#1#2\and#3\and{\@andlistc{#2}\@ifnotempty{#3}{%
  \@andlistb#1{#3}}}
\def\@andlistb#1#2#3#4#5\and{%
  \@ifempty{#5}{%
    \@andlistc{#2#4}%
  }{%
    \@andlistc{#1#4}\@andlistb{#1}{#3}{#3}{#5}%
  }}
\let\@andlistc\@iden
\newcommand{\nxandlist}[4]{%
  \def\@andlistc##1{\toks@\@xp{\the\toks@##1}}%
  \toks@{\toks@\@emptytoks \@andlista{{#1}{#2}{#3}}}%
  \the\@xp\toks@#4\and\and
  \edef#4{\the\toks@}%
  \let\@andlistc\@iden}
\newcommand{\andify}{%
  \nxandlist{\unskip, }{\unskip{} and~}{\unskip, and~}}
\def\and{\unskip{ }and \ignorespaces}
\def\maketitle{\par
  \@topnum\z@ % this prevents figures from falling at the top of page 1
  \@setcopyright
  \uppercasenonmath\shorttitle
  \ifx\@empty\shortauthors \let\shortauthors\shorttitle
  \else \andify\shortauthors \uppercasenonmath\shortauthors \fi
  \@maketitle@hook
  \begingroup
  \@maketitle
  \toks@\@xp{\shortauthors}\@temptokena\@xp{\shorttitle}%
  \edef\@tempa{\@nx\markboth{\the\toks@}{\the\@temptokena}}\@tempa
  \endgroup
  \thispagestyle{firstpage}% this sets first page specifications
  \c@footnote\z@
  \def\do##1{\let##1\relax}%
  \do\maketitle \do\@maketitle \do\title \do\@xtitle \do\@title
  \do\author \do\@xauthor \do\address \do\@xaddress
  \do\email \do\@xemail \do\curraddr \do\@xcurraddr
  \do\commby \do\@commby
  \do\dedicatory \do\@dedicatory \do\thanks \do\thankses
  \do\keywords \do\@keywords \do\subjclass \do\@subjclass
}
\def\@maketitle@hook{\global\let\@maketitle@hook\@empty}
\def\@maketitle{%
  \normalfont\normalsize
  \let\@makefnmark\relax  \let\@thefnmark\relax
  \ifx\@empty\@date\else \@footnotetext{\@setdate}\fi
  \ifx\@empty\@subjclass\else \@footnotetext{\@setsubjclass}\fi
  \ifx\@empty\@keywords\else \@footnotetext{\@setkeywords}\fi
  \ifx\@empty\thankses\else \@footnotetext{%
    \def\par{\let\par\@par}\@setthanks}\fi
  \@mkboth{\@nx\shortauthors}{\@nx\shorttitle}%
  \global\topskip42\p@\relax % 5.5pc   "   "   "     "     "
  \@settitle
  \ifx\@empty\authors \else \@setauthors \fi
  \ifx\@empty\@dedicatory
  \else
    \baselineskip18\p@
    \vtop{\centering{\footnotesize\itshape\@dedicatory\@@par}%
      \global\dimen@i\prevdepth}\prevdepth\dimen@i
  \fi
  \@setabstract
  \normalsize
  \if@titlepage
    \newpage
  \else
    \dimen@34\p@ \advance\dimen@-\baselineskip
    \vskip\dimen@\relax
  \fi
} % end \@maketitle
\AtBeginDocument{%
  \@ifundefined{publname}{%
    \let\publname\@empty
    \let\@serieslogo\@empty
  }{%
    \def\@serieslogo{%
      \vbox to\headheight{%
        \parindent\z@ \fontsize{6}{7\p@}\selectfont
        \noindent\publname\newline
        \volinfo, \pageinfo \@dateposted\newline \@PII\endgraf
        \vss
      }%
    }%
  }%
}
\AtBeginDocument{%
  \@ifundefined{volinfo}{%
    \def\volinfo{%
      Volume \currentvolume, Number \number0\currentissue,
      \currentmonth\ \currentyear
    }%
  }{}%
}
\def\issueinfo#1#2#3#4{\def\currentvolume{#1}\def\currentissue{#2}%
  \def\currentmonth{#3}\def\currentyear{#4}}
\issueinfo{00}% volume number
  {0}%        % issue number
  {Xxxx}%     % month
  {XXXX}%     % year
\def\copyrightinfo#1#2{\def\copyrightyear{#1}\def\copyrightholder{#2}}
\copyrightinfo{0000}{(copyright holder)}
\def\pagespan#1#2{\setcounter{page}{#1}%
  \ifnum\c@page<\z@ \pagenumbering{roman}\setcounter{page}{-#1}\fi
  \def\start@page{#1}\def\end@page{#2}}
\pagespan{000}{000}
\@ifundefined{pageinfo}{%
  \def\pageinfo{%
    \ifnum\start@page=\z@
      Pages 000--000
    \else
      \ifx\start@page\end@page
        Page \start@page
      \else
        Pages \start@page--\end@page
      \fi
    \fi}%
}{}
\@ifundefined{ISSN}{\def\ISSN{0000-0000}}{}
\newcommand\PII[1]{\def\@PII{#1}}
\PII{S \ISSN(XX)0000-0}
\newinsert\copyins
\skip\copyins=1.5pc                         % redefined in asl
\count\copyins=1000 % magnification factor, 1000 = 100%
\dimen\copyins=.5\textheight % maximum allowed per page
\def\@combinefloats{%
  \ifx \@toplist\@empty \else \@cflt \fi
  \ifx \@botlist\@empty \else \@cflb \fi
  \ifvoid\copyins \else \@cflci \fi
}
\def\@cflci{%
  \if\if@twocolumn \if@firstcolumn F\else T\fi\else T\fi T%
      \setbox\@outputbox\vbox{%
        \unvbox\@outputbox
        \vskip\skip\copyins
        \hbox to\columnwidth{%
          \hss\vbox to\z@{\vss\unvbox\copyins}}}%
  \fi
}
\newcommand{\abstractname}{Abstract}
\newcommand{\keywordsname}{Key words and phrases}
\newcommand{\subjclassname}{\textup{1991} Mathematics Subject
     Classification}
\def\@tempb{amsart}
\ifx\@classname\@tempb
  \newcommand{\datename}{\textit{Date}:}
\else
  \newcommand{\datename}{Received by the editors}
\fi
\def\@setdate{\datename\ \@date\@addpunct.}
\def\@setsubjclass{%
  {\itshape\subjclassname.}\enspace\@subjclass\@addpunct.}
\def\@setkeywords{%
  {\itshape \keywordsname.}\enspace \@keywords\@addpunct.}
% \def\@setthanks{\def\thanks##1{\par##1\@addpunct.}\thankses}
\newbox\abstractbox
\def\@setabstract{\@setabstracta \global\let\@setabstract\relax}
\def\titlepage{%
  \clearpage
  \thispagestyle{empty}\setcounter{page}{0}}
\def\endtitlepage{\newpage}
\def\labelenumi{\theenumi.}
\def\theenumi{\@arabic\c@enumi}
\def\labelenumii{(\theenumii)}
\def\theenumii{\@alph\c@enumii}
\def\p@enumii{\theenumi}
\def\labelenumiii{(\theenumiii)}
\def\theenumiii{\@roman\c@enumiii}
\def\p@enumiii{\theenumi(\theenumii)}
\def\labelenumiv{(\theenumiv)}
\def\theenumiv{\@Alph\c@enumiv}
\def\p@enumiv{\p@enumiii\theenumiii}
\def\labelitemi{$\m@th\bullet$}
\def\labelitemii{\bfseries --}% \upshape already done by \itemize
\def\labelitemiii{$\m@th\ast$}
\def\labelitemiv{$\m@th\cdot$}
\newenvironment{verse}{\let\\\@centercr
  \list{}{\itemsep\z@ \itemindent -1.5em\listparindent\itemindent
  \rightmargin\leftmargin \advance\leftmargin 1.5em}\item[]%
}{%
  \endlist
}
\let\endverse=\endlist % for efficiency
\newenvironment{quote}{%
  \list{}{\rightmargin\leftmargin}\item[]%
}{%
  \endlist
}
\let\endquote=\endlist % for efficiency
\def\trivlist{\parsep\parskip\@nmbrlistfalse
  \@trivlist \labelwidth\z@ \leftmargin\z@
  \itemindent\z@
  \let\@itemlabel\@empty
  \def\makelabel##1{\upshape##1}}
\renewenvironment{enumerate}{%
  \ifnum \@enumdepth >3 \@toodeep\else
      \advance\@enumdepth \@ne
      \edef\@enumctr{enum\romannumeral\the\@enumdepth}\list
      {\csname label\@enumctr\endcsname}{\usecounter
        {\@enumctr}\def\makelabel##1{\hss\llap{\upshape##1}}}\fi
}{%
  \endlist
}
\let\endenumerate=\endlist % for efficiency
\renewenvironment{itemize}{%
  \ifnum\@itemdepth>3 \@toodeep
  \else \advance\@itemdepth\@ne
    \edef\@itemitem{labelitem\romannumeral\the\@itemdepth}%
    \list{\csname\@itemitem\endcsname}%
      {\def\makelabel##1{\hss\llap{\upshape##1}}}%
  \fi
}{%
  \endlist
}
\let\enditemize=\endlist % for efficiency
\newcommand{\descriptionlabel}[1]{\hspace\labelsep \upshape\bfseries #1:}
\newenvironment{description}{\list{}{%
  \advance\leftmargini6\p@ \itemindent-12\p@
  \labelwidth\z@ \let\makelabel\descriptionlabel}%
}{
  \endlist
}
\let\enddescription=\endlist % for efficiency
\let\upn=\textup
\AtBeginDocument{%
  \settowidth\leftmargini{\labelenumi\hskip\labelsep}%
  \advance\leftmargini by \normalparindent
  \settowidth\leftmarginii{\labelenumii\hskip\labelsep}%
  \advance\leftmarginii by 6pt
  \settowidth\leftmarginiii{\labelenumiii\hskip\labelsep}%
  \advance\leftmarginiii by 6pt
  \settowidth\leftmarginiv{\labelenumiv\hskip\labelsep}%
  \advance\leftmarginiv by 10pt
  \leftmarginv=10pt
  \leftmarginvi=10pt
  \leftmargin=\leftmargini
  \labelsep=5pt
  \labelwidth=\leftmargini \advance\labelwidth-\labelsep
  \@listi}
\newskip\listisep
\listisep\smallskipamount
\def\@listI{\leftmargin\leftmargini \parsep\z@skip
  \topsep\listisep \itemsep\z@skip
  \listparindent\normalparindent}
\let\@listi\@listI
\def\@listii{\leftmargin\leftmarginii
  \labelwidth\leftmarginii \advance\labelwidth-\labelsep
  \topsep\z@skip \parsep\z@skip \partopsep\z@skip \itemsep\z@skip}
\def\@listiii{\leftmargin\leftmarginiii
  \labelwidth\leftmarginiii \advance\labelwidth-\labelsep}
\def\@listiv{\leftmargin\leftmarginiv
  \labelwidth\leftmarginiv \advance\labelwidth-\labelsep}
\def\@listv{\leftmargin\leftmarginv
  \labelwidth\leftmarginv \advance\labelwidth-\labelsep}
\def\@listvi{\leftmargin\leftmarginvi
  \labelwidth\leftmarginvi \advance\labelwidth-\labelsep}
\def\@startsection#1#2#3#4#5#6{%
\if@noskipsec \leavevmode \fi
\par \@tempskipa #4\relax
\@afterindenttrue
\ifdim\@tempskipa<\z@\@tempskipa-\@tempskipa\@afterindentfalse\fi
\if@nobreak\everypar{}\else
\addpenalty\@secpenalty\addvspace\@tempskipa\fi
\@ifstar{\@dblarg{\@sect{#1}{\@m}{#3}{#4}{#5}{#6}}}%
{\@dblarg{\@sect{#1}{#2}{#3}{#4}{#5}{#6}}}%
}
% This is in asl code, mildly ammended ynm
%\def\@sect#1#2#3#4#5#6[#7]#8{%
%  \edef\@toclevel{\ifnum#2=\@m 0\else\number#2\fi}%
%  \ifnum #2>\c@secnumdepth \let\@secnumber\@empty
%  \else \@xp\let\@xp\@secnumber\csname the#1\endcsname\fi
% \ifnum #2>\c@secnumdepth
%   \let\@svsec\@empty
% \else
%    \refstepcounter{#1}%
%    \edef\@svsec{\ifnum#2<\@m
%       \@ifundefined{#1name}{}{%
%         \ignorespaces\csname #1name\endcsname\space}\fi
%       \@nx\textup{%
%      \@nx\@secnumfont
%         \csname the#1\endcsname.}\enspace
%    }%
%  \fi
%  \@tempskipa #5\relax
%  \ifdim \@tempskipa>\z@ % then this is not a run-in section heading
%    \begingroup #6\relax
%    \@hangfrom{\hskip #3\relax\@svsec}{\interlinepenalty\@M #8\par}%
%    \endgroup
%    \ifnum#2>\@m \else \@tocwrite{#1}{#8}\fi
%  \else
%  \def\@svsechd{#6\hskip #3\@svsec
%    \@ifnotempty{#8}{\ignorespaces#8\unskip
%       \@addpunct.}%
%    \ifnum#2>\@m \else \@tocwrite{#1}{#8}\fi
%  }%
%  \fi
%  \global\@nobreaktrue
%  \@xsect{#5}}
\let\@ssect\relax
\newcounter{part}
\newcounter{section}
\newcounter{subsection}[section]
\newcounter{subsubsection}[subsection]
\newcounter{paragraph}[subsubsection]
\renewcommand\thepart          {\arabic{part}}
\renewcommand\theparagraph     {\thesubsubsection.\arabic{paragraph}}
\setcounter{secnumdepth}{3}
\def\partname{Part}
\def\part{\@startsection{part}{0}%
  \z@{\linespacing\@plus\linespacing}{.5\linespacing}%
  {\normalfont\bfseries\raggedright}}
\def\specialsection{\@startsection{section}{1}%
  \z@{\linespacing\@plus\linespacing}{.5\linespacing}%
  {\normalfont\centering}}
\def\paragraph{\@startsection{paragraph}{4}%
  \z@\z@{-\fontdimen2\font}%
  \normalfont}
\def\subparagraph{\@startsection{subparagraph}{5}%
  \z@\z@{-\fontdimen2\font}%
  \normalfont}
\def\appendix{\par\c@section\z@ \c@subsection\z@
   \let\sectionname\appendixname
   \def\thesection{\@Alph\c@section}}
\def\appendixname{Appendix}
\def\@Roman#1{\@xp\@slowromancap
  \romannumeral#1@}%
\def\@slowromancap#1{\ifx @#1% then terminate
  \else
    \if i#1I\else\if v#1V\else\if x#1X\else\if l#1L\else\if
    c#1C\else\if m#1M\else#1\fi\fi\fi\fi\fi\fi
    \@xp\@slowromancap
  \fi
}
\newcommand{\@pnumwidth}{1.6em}
\newcommand{\@tocrmarg}{2.6em}
\setcounter{tocdepth}{2}
\def\@starttoc#1#2{\begingroup
  \par\removelastskip\vskip\z@skip
  \@startsection{}\@M\z@{\linespacing\@plus\linespacing}%
    {.5\linespacing}{\centering\scshape}{#2}%
  \ifx\contentsname#2%
  \else \addcontentsline{toc}{section}{#2}\fi
  \makeatletter
  \@input{\jobname.#1}%
  \if@filesw
    \@xp\newwrite\csname tf@#1\endcsname
    \immediate\@xp\openout\csname tf@#1\endcsname \jobname.#1\relax
  \fi
  \global\@nobreakfalse \endgroup
  \addvspace{32\p@\@plus14\p@}%
  \let\tableofcontents\relax
}
\def\contentsname{Contents}
\def\listfigurename{List of Figures}
\def\listtablename{List of Tables}
\def\tableofcontents{\@starttoc{toc}\contentsname}
\def\listoffigures{\@starttoc{lof}\listfigurename}
\def\listoftables{\@starttoc{lot}\listtablename}
\AtBeginDocument{%
  \@for\@tempa:=-1,0,1,2,3\do{%
    \@ifundefined{r@tocindent\@tempa}{%
      \@xp\gdef\csname r@tocindent\@tempa\endcsname{0pt}}{}%
  }%
}
\def\@writetocindents{%
  \begingroup
  \@for\@tempa:=-1,0,1,2,3\do{%
    \immediate\write\@auxout{%
      \string\newlabel{tocindent\@tempa}{%
        \csname r@tocindent\@tempa\endcsname}}%
  }%
  \endgroup}
\AtEndDocument{\@writetocindents}

\let\indentlabel\@empty
\def\@tochangmeasure#1{\sbox\z@{#1}%
  \ifdim\wd\z@>\csname r@tocindent\@toclevel\endcsname\relax
    \@xp\xdef\csname r@tocindent\@toclevel\endcsname{\the\wd\z@}%
  \fi
}
\def\@toclevel{0}
\def\@tocline#1#2#3#4#5#6#7{\relax
  \ifnum #1>\c@tocdepth % then omit
  \else
    \par \addpenalty\@secpenalty\addvspace{#2}%
    \begingroup \hyphenpenalty\@M
    \@ifempty{#4}{%
      \@tempdima\csname r@tocindent\number#1\endcsname\relax
    }{%
      \@tempdima#4\relax
    }%
    \parindent\z@ \leftskip#3\relax \advance\leftskip\@tempdima\relax
    \rightskip\@pnumwidth plus1em \parfillskip-\@pnumwidth
    #5\leavevmode\hskip-\@tempdima #6\relax
    \hfil\hbox to\@pnumwidth{\@tocpagenum{#7}}\par
    \nobreak
    \endgroup
  \fi}
\def\@tocpagenum#1{\hss{\mdseries #1}}
\def\@tocwrite#1{\@xp\@tocwriteb\csname toc#1\endcsname{#1}}
\def\@tocwriteb#1#2#3{%
  \begingroup
    \def\@tocline##1##2##3##4##5##6{%
      \ifnum##1>\c@tocdepth
      \else \sbox\z@{##5\let\indentlabel\@tochangmeasure##6}\fi}%
    \csname l@#2\endcsname{#1{\csname#2name\endcsname}{\@secnumber}{}}%
  \endgroup
  \addcontentsline{toc}{#2}%
    {\protect#1{\csname#2name\endcsname}{\@secnumber}{#3}}}
\def\l@section{\@tocline{1}{0pt}{1pc}{}{}}
\newcommand{\tocsection}[3]{%
  \indentlabel{\@ifnotempty{#2}{\ignorespaces#1 #2.\quad}}#3}
\def\l@subsection{\@tocline{2}{0pt}{1pc}{5pc}{}}
\let\tocsubsection\tocsection
\def\l@subsubsection{\@tocline{3}{0pt}{1pc}{7pc}{}}
\let\tocsubsubsection\tocsection
\def\l@part{\@tocline{-1}{12pt plus2pt}{0pt}{}{\bfseries}}
\let\tocpart\tocsection
\def\l@chapter{\@tocline{0}{8pt plus1pt}{0pt}{}{}}
\let\tocchapter\tocsection
\let\tocappendix\tocchapter
\def\l@figure{\@tocline{0}{3pt plus2pt}{0pt}{}{}}
\let\l@table=\l@figure
\def\refname{References}               % redefined in asl
\def\bibname{Bibliography}
\def\bibliographystyle#1{%
   \if@filesw\immediate\write\@auxout
    {\string\bibstyle{#1}}\fi
        \def\@tempa{#1}%
        \def\@tempb{amsplain}%
        \def\@tempc{}%
        \ifx\@tempa\@tempb
                \def\@biblabel##1{##1.}%
                \def\bibsetup{}%
        \else
                \def\bibsetup{\labelsep6\p@}%
        \ifx\@tempa\@tempc
                \def\@biblabel##1{}%
                \def\bibsetup{\labelwidth\z@ \leftmargin24\p@
                \itemindent-24\p@
                          \labelsep\z@ }%
        \fi
\fi}
%\newenvironment{thebibliography}[1]{%
%  \@xp\section\@xp*\@xp{\refname}%
%  \normalfont\footnotesize\labelsep .5em\relax
%  \renewcommand\theenumiv{\arabic{enumiv}}\let\p@enumiv\@empty
%  \list{\@biblabel{\theenumiv}}{\settowidth\labelwidth{\@biblabel{#1}}%
%    \leftmargin\labelwidth \advance\leftmargin\labelsep
%    \usecounter{enumiv}}%
%  \sloppy \clubpenalty\@M \widowpenalty\clubpenalty
%  \sfcode`\.=\@m
%}{%
%  \def\@noitemerr{\@latex@warning{Empty `thebibliography' environment}}%
%  \endlist
%}
\def\newblock{}
\newcommand\MR[1]{\relax\ifhmode\unskip\spacefactor3000 \space\fi
  \def\@tempa##1:##2:##3\@nil{%
    \ifx @##2\@empty##1\else\textbf{##1:}##2\fi}%
  \MRhref{#1}{MR \@tempa#1:@:\@nil}}
\newcommand\URL{\begingroup
  \def\@sverb##1{%
    \def\@tempa####1##1{\@URL{####1}\egroup\endgroup}%
    \@tempa}%
  \verb}
\let\URLhref\@gobble
\def\@URL#1{\URLhref{#1}#1}
\newif\if@restonecol
\def\theindex{\@restonecoltrue\if@twocolumn\@restonecolfalse\fi
  \columnseprule\z@ \columnsep 35\p@
  \twocolumn[\@xp\section\@xp*\@xp{\indexname}]%
  \thispagestyle{plain}%
  \let\item\@idxitem
  \parindent\z@  \parskip\z@\@plus.3\p@\relax
  \footnotesize}
\def\indexname{Index}
\def\@idxitem{\par\hangindent 2em}
\def\subitem{\par\hangindent 2em\hspace*{1em}}
\def\subsubitem{\par\hangindent 3em\hspace*{2em}}
\def\endtheindex{\if@restonecol\onecolumn\else\clearpage\fi}
\def\indexspace{\par\bigskip}
\def\footnoterule{\kern-.4\p@
        \hrule\@width 5pc\kern11\p@\kern-\footnotesep}
\def\@makefnmark{\hbox{$\m@th^{\@thefnmark}$}}
\def\@makefntext{\indent\@makefnmark}
\long\def\@footnotetext#1{\insert\footins{%
  \normalfont\footnotesize
  \interlinepenalty\interfootnotelinepenalty
  \splittopskip\footnotesep \splitmaxdepth \dp\strutbox
  \floatingpenalty\@MM \hsize\columnwidth
  \@parboxrestore \parindent\normalparindent \sloppy
  \edef\@currentlabel{\p@footnote\@thefnmark}%
  \@makefntext{\rule\z@\footnotesep\ignorespaces#1\unskip\strut\par}}}
\hfuzz=1pt \vfuzz=\hfuzz
\def\sloppy{\tolerance9999 \emergencystretch 3em\relax}
\setcounter{topnumber}{4}
\setcounter{bottomnumber}{4}
\setcounter{totalnumber}{4}
\setcounter{dbltopnumber}{4}
\renewcommand{\topfraction}{.97}
\renewcommand{\bottomfraction}{.97}
\renewcommand{\textfraction}{.03}
\renewcommand{\floatpagefraction}{.9}
\renewcommand{\dbltopfraction}{.97}
\renewcommand{\dblfloatpagefraction}{.9}
\setlength{\floatsep}{12pt plus 6pt minus 4pt}
\setlength{\textfloatsep}{15pt plus 8pt minus 5pt}
\setlength{\intextsep}{12pt plus 6pt minus 4pt}
\setlength{\dblfloatsep}{12pt plus 6pt minus 4pt}
\setlength{\dbltextfloatsep}{15pt plus 8pt minus 5pt}
\setlength{\@fptop}{0pt}% removed ``plus 1fil''
\setlength{\@fpsep}{8pt}% removed ``plus 2fil''
\setlength{\@fpbot}{0pt plus 1fil}
\setlength{\@dblfptop}{0pt}% removed ``plus 1fil''
\setlength{\@dblfpsep}{8pt}% removed ``plus 2fil''
\setlength{\@dblfpbot}{0pt plus 1fil}
\newcommand{\fps@figure}{tbp}
\newcommand{\fps@table}{tbp}
\newcounter{figure}
\def\@captionheadfont{\scshape}
\def\@captionfont{\normalfont}
\def\ftype@figure{1}
\def\ext@figure{lof}
\def\fnum@figure{\figurename\ \thefigure}
\def\figurename{Figure}
\newenvironment{figure}{%
  \@float{figure}%
}{%
  \end@float
}
\newcounter{table}
\def\ftype@table{2}
\def\ext@table{lot}
\def\fnum@table{\tablename\ \thetable}
\def\tablename{Table}
\newenvironment{table}{%
  \@float{table}%
}{%
  \end@float
}
\def\@floatboxreset{\global\@minipagefalse \centering}
\long\def\@makecaption#1#2{%
  \setbox\@tempboxa\vbox{\color@setgroup
    \advance\hsize-2\captionindent\noindent
    \@captionfont\@captionheadfont#1\@xp\@ifnotempty\@xp
        {\@cdr#2\@nil}{.\@captionfont\upshape\enspace#2}%
    \unskip\kern-2\captionindent\par
    \global\setbox\@ne\lastbox\color@endgroup}%
  \ifhbox\@ne % the normal case
    \setbox\@ne\hbox{\unhbox\@ne\unskip\unskip\unpenalty\unkern}%
  \fi
  \ifdim\wd\@tempboxa=\z@ % this means caption will fit on one line
    \setbox\@ne\hbox to\columnwidth{\hss\kern-2\captionindent\box\@ne\hss}%
  \else % tempboxa contained more than one line
    \setbox\@ne\vbox{\unvbox\@tempboxa\parskip\z@skip
        \noindent\unhbox\@ne\advance\hsize-2\captionindent\par}%
\fi
  \ifnum\@tempcnta<64 % if the float IS a figure...
    \addvspace\abovecaptionskip
    \moveright\captionindent\box\@ne
  \else % if the float IS NOT a figure...
    \moveright\captionindent\box\@ne
    \nobreak
    \vskip\belowcaptionskip
  \fi
\relax
}
\newskip\abovecaptionskip \abovecaptionskip=12pt \relax
\newskip\belowcaptionskip \belowcaptionskip=12pt \relax
\newdimen\captionindent \captionindent=3pc

%%%%%%%%%%%%%%%%%%%%%%% code from amsthm.sty 
\newcommand{\theoremstyle}[1]{%
  \@ifundefined{th@#1}{%
    \PackageWarning{amsthm}{Unknown theoremstyle `#1'}%
    \thm@style{plain}%
  }{%
    \thm@style{#1}%
  }%
}
\newtoks\thm@style
\thm@style{plain}
\newtoks\thm@bodyfont
\thm@bodyfont{\itshape}
\newtoks\thm@headfont
\thm@headfont{\bfseries}
\newtoks\thm@notefont
\thm@notefont{}
\newtoks\thm@headpunct
\thm@headpunct{.}
\newskip\thm@preskip \thm@preskip\topsep
\newskip\thm@postskip \thm@postskip\topsep
\renewcommand{\newtheorem}{\@ifstar{\@xnthm *}{\@xnthm \relax}}
\def\@xnthm#1#2{%
  \let\@tempa\relax
  \@xp\@ifdefinable\csname #2\endcsname{%
    \global\@xp\let\csname end#2\endcsname\@endtheorem
    \ifx *#1% unnumbered, need to get one more mandatory arg
      \edef\@tempa##1{%
        \gdef\@xp\@nx\csname#2\endcsname{%
          \@nx\@thm{\@xp\@nx\csname th@\the\thm@style\endcsname}%
            {}{##1}}}%
    \else % numbered theorem, need to check for optional arg
      \def\@tempa{\@oparg{\@ynthm{#2}}[]}%
    \fi
  }%
  \@tempa
}
\def\@ynthm#1[#2]#3{%
  \ifx\relax#2\relax
    \def\@tempa{\@oparg{\@xthm{#1}{#3}}[]}%
  \else
    \@ifundefined{c@#2}{%
      \def\@tempa{\@nocounterr{#2}}%
    }{%
      \@xp\xdef\csname the#1\endcsname{\@xp\@nx\csname the#2\endcsname}%
      \toks@{#3}%
      \@xp\xdef\csname#1\endcsname{%
        \@nx\@thm{%
          \let\@nx\thm@swap
            \if S\thm@swap\@nx\@firstoftwo\else\@nx\@gobble\fi
          \@xp\@nx\csname th@\the\thm@style\endcsname}%
            {#2}{\the\toks@}}%
      \let\@tempa\relax
    }%
  \fi
  \@tempa
}
\def\@xthm#1#2[#3]{%
  \ifx\relax#3\relax
    \newcounter{#1}%
  \else
    \newcounter{#1}[#3]%
    \@xp\xdef\csname the#1\endcsname{\@xp\@nx\csname the#3\endcsname
      \@thmcountersep\@thmcounter{#1}}%
  \fi
  \toks@{#2}%
  \@xp\xdef\csname#1\endcsname{%
    \@nx\@thm{%
      \let\@nx\thm@swap
        \if S\thm@swap\@nx\@firstoftwo\else\@nx\@gobble\fi
      \@xp\@nx\csname th@\the\thm@style\endcsname}%
      {#1}{\the\toks@}}%
}
\let\@ythm\relax
\let\thmname\@iden \let\thmnote\@iden \let\thmnumber\@iden
\providecommand\@upn{\textup}
\def\thmhead@plain#1#2#3{%
  \thmname{#1}\thmnumber{\@ifnotempty{#1}{ }#2}%
  \thmnote{ {\the\thm@notefont(#3)}}}
\let\thmhead\thmhead@plain
\def\swappedhead#1#2#3{%
  \thmnumber{#2}\thmname{\@ifnotempty{#2}{. }#1}%
  \thmnote{ {\the\thm@notefont(#3)}}}
\let\thmheadnl\relax
\def\@begintheorem#1#2[#3]{%
  \item[\normalfont % reset in case body font is abnormal
  \hskip\labelsep
  \the\thm@headfont
  \thm@indent
  \@ifempty{#1}{\let\thmname\@gobble}{\let\thmname\@iden}%
  \@ifempty{#2}{\let\thmnumber\@gobble}{\let\thmnumber\@iden}%
  \@ifempty{#3}{\let\thmnote\@gobble}{\let\thmnote\@iden}%
  \thm@swap\swappedhead\thmhead{#1}{#2}{#3}%
  \the\thm@headpunct]%
  \@restorelabelsep
  \thmheadnl % possibly a newline.
  \ignorespaces}
\def\nonslanted{\relax
  \@xp\let\@xp\@tempa\csname\f@shape shape\endcsname
  \ifx\@tempa\itshape\upshape
  \else\ifx\@tempa\slshape\upshape\fi\fi}
\def\swapnumbers{\edef\thm@swap{\if S\thm@swap N\else S\fi}}
\def\thm@swap{N}%
\let\@opargbegintheorem\relax
\def\th@plain{%
  \itshape % body font
}
\def\th@definition{%
  \normalfont % body font
}
\def\th@remark{%
  \thm@headfont{\itshape}%
  \normalfont % body font
  \thm@preskip\topsep
  \divide\thm@preskip\tw@
  \thm@postskip\thm@preskip
}
\def\@endtheorem{\endtrivlist\@endpefalse }
\newcommand{\newtheoremstyle}[9]{%
  \@ifempty{#5}{\dimen@\z@skip}{\dimen@#5\relax}%
  \ifdim\dimen@=\z@
    \toks@{#4\let\thm@indent\noindent}%
  \else
    \toks@{#4\def\thm@indent{\noindent\hbox to#5{}}}%
  \fi
  \def\@tempa{#8}\ifx\space\@tempa
    \toks@\@xp{\the\toks@ \labelsep\fontdimen\tw@\font\relax}%
  \else
    \def\@tempb{\newline}%
    \ifx\@tempb\@tempa
      \toks@\@xp{\the\toks@ \labelsep\z@skip
        \def\thmheadnl{%
          \@noskipsectrue \global\@nobreaktrue
          \everypar{\global\@minipagefalse \global\@newlistfalse
            \global\@inlabelfalse \global\@nobreakfalse
            {\setbox\z@\lastbox}\box\@labels\par
            \nobreak\vskip-\parskip
            \everypar{}\noindent}}%
      }%
    \else
      \toks@\@xp{\the\toks@ \labelsep#8\relax}%
    \fi
  \fi
  \begingroup \th@plain % to set \thm@preskip and postskip
  \@defaultunits\@tempskipa#2\thm@preskip\relax\@nnil
  \@defaultunits\@tempskipb#3\thm@postskip\relax\@nnil
  \xdef\@gtempa{\thm@preskip\the\@tempskipa
    \thm@postskip\the\@tempskipb\relax}%
  \endgroup
  \@temptokena\@xp{\@gtempa
    \thm@headfont{#6}\thm@headpunct{#7}%
  }%
  \@ifempty{#9}{%
    \let\thmhead\thmhead@plain
  }{%
    \@namedef{thmhead@#1}##1##2##3{#9}%
    \@temptokena\@xp{\the\@temptokena
      \@xp\let\@xp\thmhead\csname thmhead@#1\endcsname}%
  }%
  \@xp\xdef\csname th@#1\endcsname{\the\toks@ \the\@temptokena}%
}
\DeclareRobustCommand{\qed}{%
  \ifmmode % if math mode, assume display: omit penalty etc.
  \else \leavevmode\unskip\penalty9999 \hbox{}\nobreak\hfill
  \fi
  \quad\hbox{\qedsymbol}}
\newcommand{\openbox}{\leavevmode
  \hbox to.77778em{%
  \hfil\vrule
  \vbox to.675em{\hrule width.6em\vfil\hrule}%
  \vrule\hfil}}
%%%%%%%%%%%%%%%%%%%%%%% end of code from amsthm.sty

\def\@swapped#1#2{#2%
  \@ifnotempty{#1}{\@addpunct{.}\quad#1\unskip}}
\def\thmhead@plain#1#2#3{%
  \thmname{#1}\thmnumber{\@ifnotempty{#1}{ }\@upn{#2}}%
  \thmnote{ \textmd{\upshape(#3)}}}
\def\swappedhead@plain#1#2#3{%
  \thmnumber{\@upn{#2}}\thmname{\@ifnotempty{#2}{. }#1}%
  \thmnote{ \textmd{\upshape(#3)}}}
\def\th@plain{%
  \let\thmhead\thmhead@plain \let\swappedhead\swappedhead@plain
  \thm@preskip.5\baselineskip\@plus.2\baselineskip
                                    \@minus.2\baselineskip
  \thm@postskip\thm@preskip
  \itshape
}
\def\th@definition{%
  \let\thmhead\thmhead@plain \let\swappedhead\swappedhead@plain
  \thm@preskip.5\baselineskip\@plus.2\baselineskip
                                    \@minus.2\baselineskip
  \thm@postskip\thm@preskip
  \upshape
}
\def\th@remark{%
  \thm@headfont{\itshape}% heading font bold
  \let\thmhead\thmhead@plain \let\swappedhead\swappedhead@plain
  \thm@preskip.5\baselineskip\@plus.2\baselineskip
                                    \@minus.2\baselineskip
  \thm@postskip\thm@preskip
  \upshape
}
\if@compatibility
\let\@newpf\proof \let\proof\relax \let\endproof\relax
\newenvironment{pf}{\@newpf[\proofname]}{\qed\endtrivlist}
\newenvironment{pf*}[1]{\@newpf[#1]}{\qed\endtrivlist}
\fi
\def\nonbreakingspace{\unskip\nobreak\ \ignorespaces}
\def~{\protect\nonbreakingspace}
\def\@biblabel#1{\@ifnotempty{#1}{[#1]}}
\def\@cite#1#2{{%
  \m@th\upshape\mdseries[{#1\if@tempswa, #2\fi}]}}
\@ifundefined{cite }{%
  \expandafter\let\csname cite \endcsname\cite
  \edef\cite{\@nx\protect\@xp\@nx\csname cite \endcsname}%
}{}
\def\fullwidthdisplay{\displayindent\z@ \displaywidth\columnwidth}
\edef\@tempa{\noexpand\fullwidthdisplay\the\everydisplay}
\everydisplay\expandafter{\@tempa}
\newcommand\seename{see also}%
\newcommand\see[2]{{\em \seename\/} #1}%
\newcommand\printindex{\@input{\jobname.ind}}%
\DeclareRobustCommand\textprime{\leavevmode
  \raise.8ex\hbox{\check@mathfonts\the\scriptfont2 \char48 }}
\hyphenation{acad-e-my acad-e-mies af-ter-thought anom-aly anom-alies
an-ti-deriv-a-tive an-tin-o-my an-tin-o-mies apoth-e-o-ses
apoth-e-o-sis ap-pen-dix ar-che-typ-al as-sign-a-ble as-sist-ant-ship
as-ymp-tot-ic asyn-chro-nous at-trib-uted at-trib-ut-able bank-rupt
bank-rupt-cy bi-dif-fer-en-tial blue-print busier busiest
cat-a-stroph-ic cat-a-stroph-i-cally con-gress cross-hatched data-base
de-fin-i-tive de-riv-a-tive dis-trib-ute dri-ver dri-vers eco-nom-ics
econ-o-mist elit-ist equi-vari-ant ex-quis-ite ex-tra-or-di-nary
flow-chart for-mi-da-ble forth-right friv-o-lous ge-o-des-ic
ge-o-det-ic geo-met-ric griev-ance griev-ous griev-ous-ly
hexa-dec-i-mal ho-lo-no-my ho-mo-thetic ideals idio-syn-crasy
in-fin-ite-ly in-fin-i-tes-i-mal ir-rev-o-ca-ble key-stroke
lam-en-ta-ble light-weight mal-a-prop-ism man-u-script mar-gin-al
meta-bol-ic me-tab-o-lism meta-lan-guage me-trop-o-lis
met-ro-pol-i-tan mi-nut-est mol-e-cule mono-chrome mono-pole
mo-nop-oly mono-spline mo-not-o-nous mul-ti-fac-eted mul-ti-plic-able
non-euclid-ean non-iso-mor-phic non-smooth par-a-digm par-a-bol-ic
pa-rab-o-loid pa-ram-e-trize para-mount pen-ta-gon phe-nom-e-non
post-script pre-am-ble pro-ce-dur-al pro-hib-i-tive pro-hib-i-tive-ly
pseu-do-dif-fer-en-tial pseu-do-fi-nite pseu-do-nym qua-drat-ic
quad-ra-ture qua-si-smooth qua-si-sta-tion-ary qua-si-tri-an-gu-lar
quin-tes-sence quin-tes-sen-tial re-arrange-ment rec-tan-gle
ret-ri-bu-tion retro-fit retro-fit-ted right-eous right-eous-ness
ro-bot ro-bot-ics sched-ul-ing se-mes-ter semi-def-i-nite
semi-ho-mo-thet-ic set-up se-vere-ly side-step sov-er-eign spe-cious
spher-oid spher-oid-al star-tling star-tling-ly sta-tis-tics
sto-chas-tic straight-est strange-ness strat-a-gem strong-hold
sum-ma-ble symp-to-matic syn-chro-nous topo-graph-i-cal tra-vers-a-ble
tra-ver-sal tra-ver-sals treach-ery turn-around un-at-tached
un-err-ing-ly white-space wide-spread wing-spread wretch-ed
wretch-ed-ly Eng-lish Euler-ian Feb-ru-ary Gauss-ian
Hamil-ton-ian Her-mit-ian Jan-u-ary Japan-ese Kor-te-weg
Le-gendre Mar-kov-ian Noe-ther-ian No-vem-ber Rie-mann-ian Sep-tem-ber}
\def\calclayout{\advance\textheight -\headheight
  \advance\textheight -\headsep
  \oddsidemargin\paperwidth
  \advance\oddsidemargin -\textwidth
  \divide\oddsidemargin\tw@
  \ifdim\oddsidemargin<.5truein \oddsidemargin.5truein \fi
  \advance\oddsidemargin -1truein
  \evensidemargin\oddsidemargin
  \topmargin\paperheight \advance\topmargin -\textheight
  \advance\topmargin -\headheight \advance\topmargin -\headsep
  \divide\topmargin\tw@
  \ifdim\topmargin<.5truein \topmargin.5truein \fi
  \advance\topmargin -1truein\relax
}
\calclayout % initialize
\pagenumbering{arabic}
\thispagestyle{plain}

%%%%%%%%%%%%%%%%%%%%%%% code from asl.sty

\let\oldcal\mathcal              % so mathcal is always available

% To make truescript give math cal with CMR fonts
\def\truescript#1{\ifxfonts\truescriptalphabet{#1}%
\else\oldcal{#1}\fi}

%------------------------------- Basic dimension defaults
\textheight=48pc          % = 19.395 cm
\textwidth=29pc           % = 12.227 cm
\headsep=12pt
\headheight=6pt
\topskip=12pt
\footskip=24pt                    % It is 12pt in amsart.cls
\parskip=0pt
\parindent=1em \normalparindent\parindent
\skip\copyins=2pc                 % .5pc larger than in amsart.cls

\floatsep=\baselineskip           % Make these smaller, still not rigid
\textfloatsep=\floatsep           % occassionally they need to be set rigid
\intextsep=\floatsep

% These have been tighter, but always needed fixing.
% Here are the values in amsart:
% \floatsep 24pt plus 2pt minus 2pt
% \textfloatsep 24pt plus 2pt minus 2pt
% \intextsep 24pt plus 2pt minus 2pt

\calclayout % initialize again

% Fudgefactor, from jsl-l
\advance\textheight by.5\normalbaselineskip
\divide\textheight by\normalbaselineskip
\multiply\textheight by\normalbaselineskip

%------------------------------- Logo and Copyright ifs

\newif\ifLogoOn
\LogoOnfalse
\def\LogoOn{\global\LogoOntrue}

% Set the fontsize of the logo larger for mainsize 11 or more
\def\@logofontsize{\ifnum\@mainsize=10 \fontsize{6}{9\p@}
   \else \fontsize{7}{10\p@}\fi}%

\newif\ifCopyRight
\CopyRightfalse
\def\CopyRightOn{\global\CopyRighttrue}


%------------------------------- The default is JSL with CMR 
%                                fonts and no logo or copyright

\def\ISSN{0022-4812}
\def\journalname{The Journal of Symbolic Logic}

% The following forms are used to compute the length of the logobox
\def\shortjournalname{The Journal of Symbolic Logic}
\def\longjournalname{The \hfil Journal \hfil of \hfil Symbolic \hfil Logic}
\AtBeginDocument{\ifLogoOn\@setlogo\fi}

%------------------------------- jsl option

\DeclareOption{jsl}{%
\LogoOn
\CopyRightOn
\def\jslname{this \textsc{Journal}} % for the bibliography
} % End \DeclareOption{jsl}

%------------------------------- xfonts option, uses the xgt fonts
\newif\ifxfonts  
\xfontsfalse

\DeclareOption{xfonts}{%
\xfontstrue
\renewcommand{\sfdefault}{cmss}
\renewcommand{\rmdefault}{xgt}
\renewcommand{\ttdefault}{cmtt}

\DeclareMathAlphabet{\mathrm}{OT1}{xgt}{m}{n}
\DeclareMathAlphabet{\mathit}{OT1}{xgt}{m}{it}
\DeclareMathAlphabet{\mathbf}{OT1}{xgt}{b}{it}

\DeclareSymbolFont{letters}{OML}{xgt}{m}{it}
\SetSymbolFont{letters}{bold}{OML}{xgt}{b}{it}
\DeclareMathAlphabet{\truescriptalphabet}{OT1}{xgt}{m}{scr}

\SetSymbolFont{operators}{normal}{OT1}{xgt}{m}{n}
\SetSymbolFont{operators}{bold}{OT1}{xgt}{b}{n}

% Must redefine \boldsymbol to correct an error in the .vf file
\let\oldboldsymbol=\boldsymbol
\def\boldsymbol#1{\ifx#1\Phi\oldboldsymbol{\Ypsilon}%
\else\ifx#1\Psi\oldboldsymbol{\Phi}%
\else\ifx#1\Ypsilon\oldboldsymbol{\Psi}\else\oldboldsymbol{#1}%
\fi\fi\fi}

% This allows the script font with all options
\newcommand{\script}{\normalfont\fontshape{scr}\selectfont}

% Use the fontshape th for theorems
% This font uses italicizes all but punctuation and numerals
\DeclareRobustCommand\xitshape{\fontshape{th}\selectfont}
\def\th@plain{\xitshape}

% For use in \textth{...} outside theorems (rare), Brian
\DeclareTextFontCommand{\textth}{\xitshape}

% Fix the lack of \l in small caps in the xgt fonts
% by using the CMR small caps \l
\def\@sc{sc}
\let\@polishl=\l
\def\l{\ifx\f@shape\@sc \raisebox{.06ex}{\fontfamily{cmr}\selectfont%
\@polishl}\else\@polishl\fi}

}% end \DeclareOption{xfonts}

%------------------------------- xjsl option, jsl with Monotype Times fonts

\DeclareOption{xjsl}{%
\ExecuteOptions{jsl,xfonts}
\let\mathcal=\truescriptalphabet    % \mathcal comes out script
                                    % \oldcal gives cal
}% end \DeclareOption{xjsl}

%---------------------------------- bsl option, bsl with cmr fonts

\DeclareOption{bsl}{%
% We want Option 11pt to be executed.
  \def\@mainsize{11}\def\@ptsize{1}%
  \def\@typesizes{%
    \or{6}{7}\or{7}{8}\or{8}{10}\or{9}{11}\or{10}{12}%
    \or{\@xipt}{13}% normalsize
    \or{\@xiipt}{14}\or{\@xivpt}{17}\or{\@xviipt}{20}%
    \or{\@xxpt}{24}\or{\@xxvpt}{30}}%
  \normalsize \linespacing=\baselineskip
\def\ISSN{1079-8986}
\def\journalname{The Bulletin of Symbolic Logic}
\def\shortjournalname{The Bulletinof SymbolicLogic}
\def\longjournalname{The \hfil Bulletin \hfil of \hfil Symbolic \hfil
Logic}
\def\bslname{this \textsc{Bulletin}}  % for the bibliography
} % end \DeclareOption{bsl}

%------------------------------- xbsl option, bsl with Monotype Times fonts

\DeclareOption{xbsl}{%
\ExecuteOptions{xfonts,bsl}
\LogoOn
\CopyRightOn
} % end \DeclareOption{xbsl}

%------------------------------- meeting option

\DeclareOption{meeting}{%
\ExecuteOptions{bsl}
\LogoOn
\CopyRightOn
% Default sizes, initialization.
% Assumes that we are setting at 11pt.
\def\AbstractsOn{%
        \normalfont
        \let\large=\small           % This is 10/12%
       \let\normalsize=\Small       % {9}{11\p@}
      \let\small=\scriptsize        % This is OK, 8/10
     \let\footnotesize=\scriptsize  % This is OK, 8/10
    \let\scriptsize=\Tiny           % We want this to be 6/7
    \fontsize{9}{11\p@}\selectfont  % the normalsize
    \@adjustvertspacing
}
\AtBeginDocument{\AbstractsOn}

\widowpenalty=0             % This is needed, to help with pagination
\clubpenalty=0

% The subtitle is, typically, "cosponsored by ..."
% but it may be different, and it may have math in it
% We put it where the Authors go
% but allowing math and not affecting the headings

\def\subtitletext{}
\def\subtitle#1{\gdef\subtitletext{\uppercase{#1}}}

% Here there are differences in vertical space from amsart
\def\@setsubtitle{%
  \begingroup
  \trivlist
\centering\fontsize{8}{9\p@}\selectfont       % Fontsize from jsl-l
\@topsep36\p@\relax
  \advance\@topsep by -2.5\baselineskip
  \item\relax
\subtitletext
  \endtrivlist
  \endgroup
\vskip -0.5\baselineskip
}

\def\location#1{\vskip 14pt
       {\gdef\@location{, #1}           % used by aslprod.sty
\normalfont\large\bfseries\centering #1\endgraf}\nobreak
                \vskip 14\p@}

\def\conferencehead#1{\vskip 14pt
             {\normalfont\large\bfseries\centering #1\endgraf}\nobreak
                \vskip 5\p@}

% Repeat \@maketitle from amsart.cls to change one skip.
% and to replace setauthors by setsubtitle
\def\@maketitle{%
  \normalfont\normalsize
  \let\@makefnmark\relax  \let\@thefnmark\relax
  \ifx\@empty\@date\else \@footnotetext{\@setdate}\fi
  \ifx\@empty\@subjclass\else \@footnotetext{\@setsubjclass}\fi
  \ifx\@empty\@keywords\else \@footnotetext{\@setkeywords}\fi
  \ifx\@empty\thankses\else \@footnotetext{%
    \def\par{\let\par\@par}\@setthanks}\fi
  \@mkboth{\@nx\shorttitle}{\@nx\shorttitle}%
  \global\topskip42\p@\relax % 5.5pc   "   "   "     "     "
  \@settitle
\ifx\@empty\subtitletext
  \else
  \@setsubtitle
\fi
  \ifx\@empty\@dedicatory
  \else
    \baselineskip18\p@
    \vtop{\centering{\footnotesize\itshape\@dedicatory\@@par}%
      \global\dimen@i\prevdepth}\prevdepth\dimen@i
  \fi
  \@setabstract
  \normalsize
  \if@titlepage
    \newpage
  \else
%    \dimen@34\p@ \advance\dimen@-\baselineskip   % (in amsart.csl)
    \dimen@14\p@ \advance\dimen@-\baselineskip    % (from jsl-l)
    \vskip\dimen@\relax
  \fi
} % end \@maketitle

\def\blurbsig#1#2{\vspace{1pc}{\noindent\hfill #1\endgraf
        \noindent\hfill {\sc #2}}\vskip\baselineskip}

%  Resetting counters within each abstract
\def\absauth#1{%
        \setcounter{equation}{0}\setcounter{abscounter}{0}
        \vspace{8pt}\noindent
        {\llap{\raisebox{.3ex}{\small$\blacktriangleright$\hskip\labelsep}}%
         \uppercase{#1}}\unskip,}

% For proofs, redefine \absauth
\let\standardabsauth=\absauth
\def\proofcopy{\def\absauth{\newpage\standardabsauth}}

\def\meetau#1{\endgraf#1\unskip,}
\def\meettitle#1{{\it #1}\unskip.}
% For the cases where the title ends with punctuation:
\def\noperiodmeettitle#1{{\it #1}\unskip}
\def\meetemail#1{\endgraf\noindent{\it E-mail}: {\tt #1}.}
\def\affil#1{\endgraf\noindent{#1.}}
\def\msort#1{\endgraf\noindent{\it Sorting}: #1.}

\def\article#1{{\it #1}\unskip.\hspace{1em}}

% Rarely, article names end with their own punctuation:
\def\noperiodarticle#1{{\it #1}\unskip\hspace{1em}}

% Even more rarely, the period at the end of the article following the
% \hspace jumps to another line.
\def\nospacearticle#1{{\it #1.}}

% No title or spacing for REFERENCES
% and use hand-set bibliography
\oldbib
%\let\cite\oldcite
%\let\@citex\old@citex

\renewenvironment{thebibliography}[1]{%
  \vskip 2pt
  \def\and{{\normalfont \lowercase{and}\ }}%
  \list{[\@arabic\c@enumiv]\ }{%
        \leftmargin\z@ \labelwidth\z@ \itemindent12\p@
  \labelsep\z@ \usecounter{enumiv}}%
  \sloppy \clubpenalty4000\relax \widowpenalty\clubpenalty
  \sfcode`\.\@m}
  {\endlist}

% Fake theorem, to fix the theorem numbering,
% so that it starts anew with each abstract.

\newtheorem{abscounter}{abscoutner}

\newtheorem{theorem}[abscounter]{Theorem}
\newtheorem{lemma}[abscounter]{Lemma}
\newtheorem{corollary}[abscounter]{Corollary}

\theoremstyle{definition}
\newtheorem{definition}[abscounter]{Definition}
\newtheorem{example}[abscounter]{Example}

\theoremstyle{plain} % 1/25/00, in case more theorems are introduced
} % end \DeclareOption{meeting}

%-------------------------------------- xmeeting option, with xgt fonts

\DeclareOption{xmeeting}{%
\ExecuteOptions{xbsl,meeting}
} % end DeclareOption

%------------------------------- notices option

\DeclareOption{notices}{%
\ExecuteOptions{bsl}
\LogoOn
\CopyRightfalse

\AtBeginDocument{\Notices}
\papertype{end notices}
% \Notices is defined globally, as it is used in the covers

% Title is not set, may be set to anything.

\def\marginmark#1{%
\global\let\email=\nemail               % sets this after \maketitle
\global\let\urladdr=\nemail             % add so this makes sense
\vskip 4pt\noindent\llap{\raisebox{.3ex}%
{$\bullet$\hskip\labelsep}}{\sc #1}}

\def\meetingdates#1{{\em #1,}}

%% This command prints location information (if there is any) in italic
%% and adds a period at the end:
\def\meetingplace#1{{\em #1.}}

% Repeat \@maketitle from amsart.cls to change one skip.
\def\@maketitle{%
  \normalfont\normalsize
  \let\@makefnmark\relax  \let\@thefnmark\relax
  \ifx\@empty\@date\else \@footnotetext{\@setdate}\fi
  \ifx\@empty\@subjclass\else \@footnotetext{\@setsubjclass}\fi
  \ifx\@empty\@keywords\else \@footnotetext{\@setkeywords}\fi
  \ifx\@empty\thankses\else \@footnotetext{%
    \def\par{\let\par\@par}\@setthanks}\fi
  \@mkboth{\@nx\shortauthors}{\@nx\shorttitle}%
  \global\topskip42\p@\relax % 5.5pc   "   "   "     "     "
  \@settitle
  \ifx\@empty\@dedicatory
  \else
    \baselineskip18\p@
    \vtop{\centering{\footnotesize\itshape\@dedicatory\@@par}%
      \global\dimen@i\prevdepth}\prevdepth\dimen@i
  \fi
  \@setabstract
  \normalsize
  \if@titlepage
    \newpage
  \else
%    \dimen@34\p@ \advance\dimen@-\baselineskip   % (in amsart.csl)
    \dimen@24\p@ \advance\dimen@-\baselineskip    % (from jsl-l, adjusted)
    \vskip\dimen@\relax
  \fi
} % end \@maketitle

% The macros for \nemail are in the general macros below,
% because they involve changing catcodes and this does not work
% with the \ProcessOptions command.
% Here we have only the re-definition for the notices option
% \email=\nemail is set in the \marginmark command
% 

}% end \DeclareOption{notices}

%------------------------------- xnotices option, with the xgt fonts

\DeclareOption{xnotices}{%
\ExecuteOptions{notices}
\ExecuteOptions{xfonts}
} % end \DeclareOption

%------------------------------- reviews option, with the cmr fonts

\DeclareOption{reviews}{%
\ExecuteOptions{bsl}
\LogoOn
\CopyRightOn

\title{Reviews}
\papertype{reviews}
\typesetter{H. Enderton}

% Setting the sizes

\def\BookRev{%
        \normalfont
        \let\large=\small           % 10/12
       \let\normalsize=\Small       % 9/11
      \let\small=\SMALL             % 8/10
     \let\footnotesize=\SMALL       % 8/10
    \let\scriptsize=\Tiny           % 6/7
    \fontsize{9}{11\p@}\selectfont  % the normalsize
    \@adjustvertspacing
}
\AtBeginDocument{\BookRev}

\newcommand{\revaddress}[1]{\g@addto@macro\localaddresses{#1. }}%

\newcommand{\revemail}[1]{\if#1\@empty \else%
\g@addto@macro\localaddresses{%
\unskip\nobreak\hfil\penalty50%
\hskip .5em\nobreak
\hbox{#1. }\hspace*{\fill}}\fi%
\g@addto@macro\localaddresses{\par}}

\newenvironment{review}{\vskip 15pt plus 1pt minus 1pt%
\let\localaddresses\@empty%
\let\reviewers\@empty%
\let\revreviewers\@empty\let\@toctitle\@empty%
\let\earlysignature\@empty%
\let\@titleindex=0%
\let\@reviewtitle\@empty}%
{\if\earlysignature X\else\signature\fi%
\if\@toctitle\@empty\tocerror\fi}

% Macros to place the signature correctly

\def\signature{\gdef\earlysignature{X}\@doreviewers\par%
\localaddresses}
% \signature places the signature where it occurs
% (rather than at the end of the review)

%\def\@doreviewers{%
%\andify\reviewers%
%\unskip\nobreak\hfil\penalty50%
%\hskip2em\hbox{}\nobreak\hfill%
%\hbox{\sc \reviewers}\hspace*{1em}}

\def\@doreviewers{%
\andify\reviewers%
\par\rightline{\textsc{\reviewers}\hspace*{1em}}}

% error message - redo to make better
\def\tocerror{\typeout{^^J%
No toctitle!^^J\stop}}

\newcommand{\reviewer}[3][]{%
  \ifx\@empty\reviewers
    \gdef\reviewers{#2}%
  \else
    \g@addto@macro\reviewers{\protect\and#2}%
  \fi
\ifx\@empty\revreviewers
\gdef\revreviewers{#3}%
\gdef\firstreviewer{#3}
\else
\g@addto@macro\revreviewers{\and#3}%
\fi
}

\def\toctitle#1{\gdef\@toctitle{#1}}
\def\reviewtitle#1{\gdef\@titleindex{1}\gdef\@reviewtitle{#1}}

\newcommand{\makereviewheading}[1][]%
{% \ydowrite in reprod.sty
{\noindent\large\if\@titleindex0%
{{\bfseries \@toctitle}\hskip 1em{#1}\vskip 3pt}%
\else\if\@reviewtitle\empty %
\else{{\bfseries \@reviewtitle}}\fi\fi}}

% To put some extra space between the preamble and the reviews
% we introduce a command for the preamble

\long\def\revinstructions#1{{#1}\vskip 5pt plus 1pt minus 1pt}


\def\horspace{\hspace{.3em}} % to test and control the spaces

\def\revau#1{%                 % same as anau, for compatibility
    \def\and{{\normalfont and\ }}%
        \vskip 0pt plus 1pt minus 1pt
    {\sc #1}\unskip.\horspace}

\def\anau#1{%
    \def\and{{\normalfont and\ }}%
        \vskip 0pt plus 1pt minus 1pt
    {\sc #1}\unskip.\horspace}

\renewcommand{\andify}{%
  \nxandlist{\unskip, }{\unskip{} {\normalfont\lowercase{and}}~}%
{\unskip, {\normalfont\lowercase{and}}~}}

\def\and{\unskip{ }\mbox{\normalfont \lowercase{and}} \ignorespaces}


\def\book#1{{\bfseries\itshape #1}\unskip{\bfseries\itshape.}\quad}

\def\article#1{{\it #1}\unskip.\horspace}

\def\journal#1{{\bfseries\itshape #1}\unskip, }

% Repeat \@maketitle from amsart.cls to change one skip.
\def\@maketitle{%
  \normalfont\normalsize
  \let\@makefnmark\relax  \let\@thefnmark\relax
  \ifx\@empty\@date\else \@footnotetext{\@setdate}\fi
  \ifx\@empty\@subjclass\else \@footnotetext{\@setsubjclass}\fi
  \ifx\@empty\@keywords\else \@footnotetext{\@setkeywords}\fi
  \ifx\@empty\thankses\else \@footnotetext{%
    \def\par{\let\par\@par}\@setthanks}\fi
  \@mkboth{\@nx\shortauthors}{\@nx\shorttitle}%
  \global\topskip42\p@\relax % 5.5pc   "   "   "     "     "
  \@settitle
  \ifx\@empty\@dedicatory
  \else
    \baselineskip18\p@
    \vtop{\centering{\footnotesize\itshape\@dedicatory\@@par}%
      \global\dimen@i\prevdepth}\prevdepth\dimen@i
  \fi
  \normalsize
  \if@titlepage
    \newpage
  \else
%    \dimen@34\p@ \advance\dimen@-\baselineskip   % (in amsart.csl)
    \dimen@24\p@ \advance\dimen@-\baselineskip    % (from jsl-l, adjusted)
    \vskip\dimen@\relax
  \fi
} % end \@maketitle

% For the production version
\InputIfFileExists{revprod.sty}{}

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% Follow Herb's macros

%-------- Font changes

\def\bi{\bfseries\itshape}
\def\lss{\sf}
\def\bss{\sf\bfseries}

%--------

\hyphenation{Eng-lish}
\abovedisplayskip=2pt plus2pt
\belowdisplayskip=2pt plus2pt

%%%%%%%%%%%%%%%%%% revlocal.tex %%%%%%%%%%%%%%%%%%%%%%%%%%%
% Note: \normalfont does what \normalshape did before
\def\Box{\square} %\square is defined in JSL stylefile; it does not
\def\Boxs{\square\hskip 0.1em}     %reduce properly in scriptstyle.
\def\Diamond{\lozenge}
\def\lor{\mathbin{\mbox{\boldmath$\vee$}}}
\def\ifthen{\mathbin{\mbox{\boldmath$\supset$}}} % horseshoe
\def\land{\mathbin{\mbox{\boldmath$\wedge$}}}
\def\lnot{\mbox{\boldmath$\sim\mskip -2.0mu$}} %9/98
\def\amp{\mathbin{\mbox{\&}}}
\def\turnStile{\mathbin{{|\hskip -0.47em\sim}}}  % symbol |~ "snake"
\def\simr{\raise 0.5ex\hbox{$\sim$}}
\def\preorder{\underset\simr\to\sqsubset}
\def\sqr#1#2{{\vbox % was \vcenter   %%from Texbook p. 320
        {\hrule height.#2pt
    \hbox{\vrule width.#2pt height#1pt \kern#1pt
        \vrule width.#2pt}
    \hrule height.#2pt}}}
%first number is size in pts, second is thickness in tenths of pts
\def\soft{$\,^\prime\!$} %for use in italics
\def\lag{\vbox{\offinterlineskip
   \hbox{$\scriptscriptstyle\langle\!\langle$}\kern-0.1pt}
   \hskip -0.6pt plus.3pt minus.2pt~ \hskip -2.7pt}
\def\rag{~\hskip -2.7pt plus.3pt minus.2pt\vbox{\offinterlineskip
   \hbox{$\scriptscriptstyle\rangle\!\rangle$}\kern-0.1pt}}
\def\smirk{\mathrel
    {\,\mid\hskip -2.1ex\lower 1ex\hbox{$\smile$}\hskip 0.1ex }}
\def\notsmirk{\mathrel{\smirk\hskip -2.5ex / \hskip 0.5ex}}
\def\ltx{La{\hskip -0.9pt}T{\hskip -0.8pt}e{\hskip -0.6pt}X} %LaTeX
\def\sqdot{\mathbin{\vrule width0.3ex height0.3ex depth0ex}} %square dot
\def\monus{-\kern -1.6ex{\raise1.0ex\hbox{.}}\kern 1.3ex}
\def\supmonus{-\kern -1.1ex{\raise0.7ex\hbox{.}}\kern 0.5ex}
\def\polishaccent{{\family{cmr}\shape{n}\selectfont\char32}}
\def\smcl{\rlap{\polishaccent}l} %small cap Polish L

\def\fishhook{\mathbin{{\vrule width0.4ex height0.80ex  %%for eight-
    depth-0.62ex}\!{\text{\sf 3}}}}   %see 60:3 p 1009  %%point
\def\boldsquare{\hspace{0.1em}
     \mathchoice\sqr{4.8}4\sqr{4.8}4\sqr{3.6}3\sqr{2.7}2\hspace{0.1em}}

%%end of revlocal.tex%%%%%%%%%%

}% end \DeclareOption

%--------------------------------------------- xreviews option, with xfonts

\DeclareOption{xreviews}{%
\ExecuteOptions{xfonts,reviews}
}% end \DeclareOption

%--------------------------------------------- obituary option
% Typeset obits exactly as if they were articles
% except for the following:
% (1) Start with \documentclass[obituary]{amsart}
%     or \documentclass[xobituary]{amsart}
% (2) Enter the command
% \subject{Smith, George W.}
% of the person honored, in this "reversed" way.
% (4) The author(s) is typeset at the end of the paper (\rightline)
%     If you want it at some place earlier, put the command
%     \signature at the place where you want it
%     (This is sometimes needed if there are references,
%      and the signature comes before the references.)
%
% The format ends up exactly as in articles, except that
% addresses and email addresses will not print,
% and so they need not be set. (They do not hurt.)

\DeclareOption{obituary}{%
\ExecuteOptions{bsl}

% Make sure that address, email do not print
\AtBeginDocument{\let\addresses\@empty \recdate{}}

% Redefine \author so that the names come at the end
\let\obitauthors\@empty
\renewcommand{\author}[2][]{%
  \ifx\@empty\obitauthors
    \gdef\shortobitauthors{#1}\gdef\obitauthors{#2}%
  \else
    \g@addto@macro\shortobitauthors{\and#1}%
    \g@addto@macro\obitauthors{\and#2}%
  \fi
}
\renewcommand{\andify}{%
  \nxandlist{\unskip, }{\unskip{} {\normalfont\lowercase{and}}~}%
{\unskip, {\normalfont\lowercase{and}}~}}

\def\and{\unskip{ }\mbox{\normalfont \lowercase{and}} \ignorespaces}

% Macros to place the signature correctly
\let\earlysignature\@empty
\def\signature{\gdef\earlysignature{@ok}\@doobitauthors}
\AtEndDocument{\ifx\@empty\earlysignature\@doobitauthors\fi}
\def\@doobitauthors{\par\bigskip\noindent%
\andify\obitauthors
\rightline{\sc \obitauthors}}

} % end \DeclareOption{obituary}

%------------------------------- xobituary option
\DeclareOption{xobituary}{%
\ExecuteOptions{xfonts,obituary}
\LogoOn
\CopyRightOn
} % end \DeclareOption{xobituary}

%--------------------------------------------- errata option
% Follow notices style
\DeclareOption{errata}{%
\LogoOnfalse
\CopyRightfalse
\AtBeginDocument{\Notices}
% must specify xjsl or xbsl along with this, to make the difference
% After the \maketitle use
% \centerline{\uppercase{volume 3}}
% \bigskip
% Page 230.  ...
% and then a \medskip separating the different errata
} % end option errata

%------------------------------------------- conference option

\DeclareOption{conference}{%
% Use the size of LC '98
% and otherwise the JSL defaults  
\textwidth=11.9cm
\textheight=18.6cm
%\let\ps@firstpage=\ps@empty
% Redefine \ps@plain, used on the first page
% to give identification and copyright

% These must be defined for each conference
\def\copyrightyear{1000}
\def\confname{Meeting}

% We set the Conference name and copyright ionfo
% using \maketitle

\let\oldmaketitle\maketitle
\def\maketitle{\oldmaketitle\confinfo}

\long\def\confinfo{%
\begin{table}[b]%
\vbox to -\footskip{%
\begin{minipage}[t]{\textwidth}
\fontsize{7}{8pt}\selectfont%
\vskip -\baselineskip
\textbf{\confname}\\
\copyright\ \copyrightyear, 
\textsc{Association for Symbolic Logic}
\end{minipage}}
\end{table}
}

} % End DeclareOption conference

%----------------------------------------- option xconference

\DeclareOption{xconference}{%
\ExecuteOptions{conference,xfonts}
}

%----------------------------------------- option endnotes


%*********************** Changes from amsart ***********************%

% The changes from amsart are noted and commented out

%------------------------------------ email for notices

% These commands are also used in the covers,
% so they are defined for all options.

\def\Notices{%
        \normalfont
        \let\normalsize=\Small     % 9/11, per jsl-l
        \let\small=\scriptsize     % 8/10, per jsl-l
        \let\scriptsize=\Tiny      % 6/7,per jsl-l
        \fontsize{9}{11\p@}\selectfont % the normalsize
        \@adjustvertspacing}

\def\weakdot{.\linebreak[0]}
\def\weakat{@\linebreak[0]}
\def\weakcolon{:\linebreak[0]}
\def\weakslash{/\linebreak[0]}

\newcommand{\emailcatcodes}{%
  \catcode`\:=\active
  \catcode`\.=\active
  \catcode`\@=\active
  \catcode`\/=\active
}

{\emailcatcodes
\immediate \global\let:=\weakcolon
\immediate \global\let.=\weakdot
\immediate \global\let@=\weakat
\immediate \global\let/=\weakslash
}

%% The catcodes are decided when the argument is read.  Thus we need
%% to set the catcodes before reading the argument to \nemail.

%\def\nemail{\begingroup\emailcatcodes\def~{$\sim$}\xemail}
\def\nemail{\begingroup\emailcatcodes\def~{\urltilde}\xemail}
%% The group ended here was started by the \nemail command:
\def\xemail#1{\tt#1\endgroup}

% \email=\nemail is implemented by the \marginmark command,
% after \maketitle

%-------------------------------- Thanks

% Skip the . in \thanks, to allow for other punctuation (rare).
%(amsart) \def\@setthanks{\def\thanks##1{\par##1\@addpunct.}\thankses}
\def\@setthanks{\def\thanks##1{\par##1}\thankses}

%-------------------------------- Maketitle

% Title not  in boldface
\def\@settitle{\begin{center}%
  \baselineskip14\p@\relax
%(amsart)    \bfseries
\fontsize{10}{13\p@}\selectfont     % At 11pt, it is still 10pt, by jsl-l
\uppercasenonmath\@title
  \@title
  \end{center}%
}

% Vertical space adjustment, per jsl-l
\def\@setabstracta{%
  \ifvoid\abstractbox
  \else
%(amsart)    \skip@20\p@ \advance\skip@-\lastskip
    \skip@36\p@ \advance\skip@-\lastskip           % to match the top
    \advance\skip@-.5\baselineskip                 % adjust, to match
    \advance\skip@-\baselineskip \vskip\skip@
    \box\abstractbox
    \prevdepth\z@ % because \abstractbox is a vtop
  \fi
}

% Vertical space adjustment and font fixing
\def\@setauthors{%
  \begingroup
  \trivlist
%(amsart)  \centering\footnotesize                % Fix, per jsl-l
% 1998: follow amsart with footnotesize
% 1998: which is 8/10 at 10pt and 9/11 at 11pt
\centering\fontsize{7}{8\p@}\selectfont
%(amsart) \@topsep30\p@\relax
\@topsep36\p@\relax                             % increase, per jsl-l
  \advance\@topsep by -\baselineskip
  \item\relax
  \andify\authors
\uppercasenonmath\authors
  \authors
  \endtrivlist
  \endgroup
}

%-------------------------------- Topmatter

\def\recdate#1{\def\@date{#1}\def\@setdate{#1}}

% Default commands for the topmatter
% These are reset automatically in production

% They need not be entered
\def\copyrightyear{0000}
\def\copyrightyearmodC{00}
\def\paperID{0000-0000}
\def\copyrightprice{\$00.00}
\def\currentvolume{00}
\def\currentnumber{0}
\def\currentyear{0000}
\def\currentmonth{XXX}

%-------------------------------- Address, email, url

% We enrich amsart with the \twoaddreess and the \genaddr
% macros.

% For authors with two affiliations:
% \twoaddress{firstaddress}{secondaddress}{c}
% where c is the number of lines in the first address
\newcommand\addrand{{\normalfont and}}   % Same as \biband

\newcommand{\twoaddress}[4][]{\g@addto@macro\addresses{%
\address{#1}{#2 \protect\advance\parindent by -#4em\protect\\\addrand %
\protect\\ #3}}}

% For other stuff in the addreses place,
% for example "Correspondence address", or "additional address"
% The optional #1 gives the appellation
% Enter as \genaddr{Correspondence address}{....}

\newcommand{\genaddr}[2]{\g@addto@macro\addresses{\genaddr{#1}{#2}}}

% Modify amsart to allow returns in \address, and indent on them
% \curraddr gives current address as in amsart

\def\@setaddresses{\par
  \nobreak \begingroup
\footnotesize
  \def\author##1{\nobreak\addvspace\bigskipamount}%
%(amsart)  \def\\{\unskip, \ignorespaces}%
  \def\\{\unskip \newline\advance\parindent by 1em\indent}%  (and indent)
  \interlinepenalty\@M

   \def\address##1##2{\begingroup
    \par\addvspace\bigskipamount\indent
    \@ifnotempty{##1}{(\ignorespaces##1\unskip) }%
%(amsart)    {\scshape\ignorespaces##2}\par\endgroup}%
    {\ignorespaces\scriptsize\uppercase{##2}}\par\endgroup}%

  \def\curraddr##1##2{\begingroup
    \def\\{\unskip, \ignorespaces}   % use amsart style for curraddr
    \@ifnotempty{##2}{\nobreak\indent{\itshape Current address}%
      \@ifnotempty{##1}{, \ignorespaces##1\unskip}\/:\space
      ##2\par\endgroup}}%

  \def\genaddr##1##2{\begingroup
    \def\\{\unskip, \ignorespaces}   % use amsart style for curraddr
     \nobreak\indent{\textit{##1}\unskip}\/:\space
      ##2\par\endgroup}%

  \def\email##1##2{\begingroup
%(amsart)    \@ifnotempty{##2}{\nobreak\indent{\itshape E-mail address}%
    \@ifnotempty{##2}{\nobreak\indent{\itshape E-mail}%
      \@ifnotempty{##1}{, \ignorespaces##1\unskip}\/:\space
%(amsart)      \ttfamily##2\par\endgroup}}%
      \normalfont ##2\par\endgroup}}%

\def\urladdr##1##2{\begingroup
    \@ifnotempty{##2}{\nobreak\indent{\itshape URL}%
      \@ifnotempty{##1}{, \ignorespaces##1\unskip}\/:\space
%(amsart)      \ttfamily##2\par\endgroup}}%
      \normalfont ##2\par\endgroup}}%
  \addresses
  \endgroup
}

\def\urltilde{$^\sim$}            % To use in URL addresses

%------------------------------- Abstract

% The Abstract is wider, with a smaller fontsize and
% a larger baselineskip than the one in amsart.
% 1998: keep the wider width but use the larger typefont
% which is just set to \Small.

%\renewenvironment{abstract}{%
\newenvironment{abstract}{%
\newcommand{\abstractfontsize}{\ifnum\@mainsize=10 \fontsize{7}{10\p@}
   \else \fontsize{8}{11\p@}\fi}
\ifx\maketitle\relax
    \ClassWarning{\@classname}{Abstract should precede
      \protect\maketitle\space in AMS documentclasses; reported}%
  \fi
  \global\setbox\abstractbox=\vtop \bgroup
 \abstractfontsize\selectfont
  \list{}{\labelwidth\z@
    \leftmargin1.5pc \rightmargin\leftmargin
    \listparindent\normalparindent \itemindent\parindent
    \parsep\z@ \@plus\p@
    \let\fullwidthdisplay\relax
    }%
%(amsart)  \item[\hskip\labelsep\scshape\abstractname.]%
  \item[\hskip\labelsep\bfseries\abstractname.]%
}{%
  \endlist\egroup
  \ifx\@setabstract\relax \@setabstracta \fi
}

%------------------------------- Logo setting

\def\@setlogo{%
% Compute the width of the logobox
\newcommand{\datenametext}{Volumei\currentvolume iNumber
\currentnumber i\currentmonth i\currentyear}
\newcommand{\longdatenametext}%
{Volume \currentvolume,\hfill Number \currentnumber,\hfill
        \currentmonth\hfill \currentyear}
\newlength{\@datename}
\settowidth{\@datename}{\normalfont\@logofontsize\selectfont \datenametext}
\newlength{\@logoname}
\settowidth{\@logoname}{%
     \normalfont\@logofontsize\scshape\selectfont%
\journalname}
\newlength{\@logowidth}
\setlength{\@logowidth}{\ifdim\@logoname>\@datename \@logoname%
\else \@datename\fi}
% Pass the logobox to the amsart routine
    \def\@serieslogo{%
\vbox to\headheight{%
  \parindent\z@ \@logofontsize\selectfont%
  \hsize\@logowidth{\scshape\selectfont \longjournalname}\newline
\longdatenametext\newline\endgraf\vss}}
}

%------------------------------- Copyright setting

\def\@copyrightfontsize{\ifnum\@mainsize=10 \fontsize{6}{8\p@}
   \else \fontsize{7}{9\p@}\fi}%

\def\@setcopyright{%
\ifCopyRight{%
   \insert\copyins{\par\hsize\textwidth
  \parfillskip\z@ \leftskip\z@\@plus.9\textwidth
\@copyrightfontsize\normalfont\upshape
      \everypar{}%
      \vskip-\skip\copyins \nointerlineskip
      \noindent\vrule\@width\z@\@height\skip\copyins
  \copyright\ \copyrightyear, Association for Symbolic Logic\break
  \ISSN/\copyrightyearmodC/\paperID/\copyrightprice\endgraf
      \par
      \kern\z@}%
}
\else\relax
\fi
}

%------------------------------- Sectioning commands


% For conditionals 
% \iete{#1}{#2}{#3} =
%    if #1 is empty then #2 else #3.
% Uses settowidth to avoid expanding #1

\newlength\ietetest
\def\iete#1#2#3{\settowidth\ietetest{#1}%
\ifdim\ietetest=0pt{#2}\else{#3}\fi}

\def\@secnumfont{\bfseries}

% Section, has the \S symbol in \thesection
% while the deeper environments do not

\renewcommand\thesection       {\arabic{section}}
\renewcommand\thesubsection    {\thesection.\arabic{subsection}}
\renewcommand\thesubsubsection {\thesubsection.\arabic{subsubsection}}

\def\section{\@secstartsection{section}{1}%   % to treat \S in \section*
{\parindent\bfseries}{12\p@\@plus3\p@}{-.5em}
{\normalfont\bfseries}}

% The adjustment gives the right spacing for \section{}

\def\@secstartsection#1#2#3#4#5#6{%
 \if@noskipsec \leavevmode \fi
 \par \@tempskipa #4\relax
 \@afterindenttrue
 \ifdim \@tempskipa <\z@ \@tempskipa -\@tempskipa \@afterindentfalse\fi
 \if@nobreak \everypar{}\else
     \addpenalty\@secpenalty\addvspace\@tempskipa\fi
 \@ifstar{\@dblarg{\@sect{#1}{\@m}{#3}{#4}{#5}{#6}}}%
         {\@dblarg{\@sect{#1}{#2}{#3\S}{#4}{#5}{#6}}}%    % \S here
}

\def\subsection{\@startsection{subsection}{2}%
 {\parindent}{2\p@}{-.5em}%      (some vertical space above)
{\normalfont\bfseries}}

\def\subsubsection{\@startsection{subsubsection}{3}%
{\parindent}{1\p@}{-.5em}%      (same as for paragraphs in bsl)
{\normalfont\itshape}}

\def\@sect#1#2#3#4#5#6[#7]#8{%
  \edef\@toclevel{\ifnum#2=\@m 0\else\number#2\fi}%
  \ifnum #2>\c@secnumdepth \let\@secnumber\@empty
  \else \@xp\let\@xp\@secnumber\csname the#1\endcsname\fi
 \ifnum #2>\c@secnumdepth
   \let\@svsec\@empty
 \else
    \refstepcounter{#1}%
    \edef\@svsec{\ifnum#2<\@m
       \@ifundefined{#1name}{}{%
         \ignorespaces\csname #1name\endcsname\space}\fi 
       \@nx\textup{%
      \@nx\@secnumfont
          \csname the#1\endcsname.}\enspace
    }%
  \fi
  \@tempskipa #5\relax
  \ifdim \@tempskipa>\z@ % then this is not a run-in section heading
    \begingroup #6\relax
    \@hangfrom{\hskip #3\relax\@svsec}{\interlinepenalty\@M #8\par}%
    \endgroup
    \ifnum#2>\@m \else \@tocwrite{#1}{#8}\fi
  \else
   \def\@svsechd{#6\hskip #3\@svsec 
% Change the next two lines to correct horizontal spacing
% in the case of \section{}
% Use \iete rather than \@ifempty to avoid expansion
%    \@ifnotempty{#8}{\ignorespaces#8\unskip 
%    \@addpunct.}%                    
%     \iete{#8}{\hskip -.4em}{\ignorespaces#8\unskip\@addpunct.}%
    \iete{#8}{\hskip -.4em}{\ignorespaces#8\unskip\@addpunct.}%
    \ifnum#2>\@m \else \@tocwrite{#1}{#8}\fi
  }%
  \fi
  \global\@nobreaktrue
  \@xsect{#5}}

%-------------------------------------- Quotation

\newenvironment{quotation}{\list{}{%
%    \leftmargin3pc \listparindent\normalparindent
    \leftmargin1.5pc \listparindent\normalparindent
    \itemindent\z@
    \rightmargin\leftmargin \parsep\z@ \@plus\p@}%
  \item[]%
}{%
  \endlist
}
\let\endquotation=\endlist % for efficiency

%-------------------------------- Theorems

% We follow amsthm.sty for the most part.

\def\@thm#1#2#3{\normalfont
  \trivlist
  \edef\@restorelabelsep{\labelsep\the\labelsep}%
  \labelsep.5em\relax \let\thmheadnl\relax
%  \let\thm@indent\noindent                      % no indent
  \let\thm@indent\indent                         % we indent
  \let\thm@swap\@gobble
%  \thm@headfont{\bfseries}                      % heading font bold
  \thm@headfont{\scshape}                        % heading font smallcaps
  \thm@headpunct{.}% add period after heading
  \thm@notefont{\normalfont}                     % optional note font, added
  \thm@preskip\topsep
  \thm@postskip\thm@preskip
  #1% style overrides
  \@topsep \thm@preskip               % used by first \item
  \@topsepadd \thm@postskip           % used by \@endparenv
  \def\@tempa{#2}\ifx\@empty\@tempa
    \def\@tempa{\@oparg{\@begintheorem{#3}{}}[]}%
  \else
    \refstepcounter{#2}%
    \def\@tempa{\@oparg{\@begintheorem{#3}{\csname the#2\endcsname}}[]}%
  \fi
  \@tempa
}
%------------------------------------------ Proofs

\newcommand{\qedsymbol}{\mbox{$\dashv$}}
\newcommand{\noqed}{\def\qedsymbol{}}

\newcommand{\proofname}{Proof}

% \begin{proof}[Proof of Claim]
% gives {\scshape Proof of Claim}. ...
% \begin{proof}[\it Proof]
% gives a proof with the proofname in italics

\newif\ifqedneeded  % to deal with the \proofend command
\qedneededtrue
\newlength{\@back}
\setlength{\@back}{\baselineskip}
\addtolength{\@back}{\jot}

\newenvironment{proof}[1][\proofname]{\par
\let\oldqedsymbol\qedsymbol             % Store \qedsymbol
\global\qedneededtrue      % just in case \proofend was invoked
  \normalfont
%  \topsep6\p@\@plus6\p@ \trivlist      % ynm debug
\trivlist                               % debug
  \item[\hskip\labelsep\hskip\parindent\scshape
    #1\@addpunct{.}]\ignorespaces
}{%
\ifqedneeded\qed\else\global\qedneededtrue%
\vspace*{-\baselineskip}\fi\endtrivlist
\global\let\qedsymbol\oldqedsymbol      % Restore it, if it had been changed
}

% Add the environment with no period after Proof
% For construction of the form
% Proof is by induction ...

\newenvironment{proofplain}[1][\proofname]{\par
\let\oldqedsymbol\qedsymbol             % Store \qedsymbol
\global\qedneededtrue      % just in case \proofend was invoked
  \normalfont
  \topsep6\p@\@plus6\p@ \trivlist
  \item[\hskip\labelsep\hskip\parindent\scshape
%    #1\@addpunct{.}]\ignorespaces
     #1]
}{%
\ifqedneeded\qed\else\global\qedneededtrue%
\vspace*{-\baselineskip}\fi\endtrivlist
\global\let\qedsymbol\oldqedsymbol      % Restore it, if it had been changed
}

%-------------------------------------- Bibliography

\def\refname{REFERENCES}
\newcommand{\bysame}{\leavevmode\hbox{%
    \vsize.7ex \advance\vsize-.4pt
    \vrule height.7ex depth-\vsize width3em}%
  \thinspace}

\newcommand{\weaktie}{\penalty9999\spacefactor1000 \space}

\newcommand\biband{{\normalfont and}}
\newcommand\bibetal{{\normalfont et\weaktie al.}}

% Define \jslname and \bslname which are produced by asl.bst
% The options jsl and bsl redefine these
\def\jslname{{\bfit The Journal of Symbolic Logic}}
\def\bslname{{\bfit The Bulletin of Symbolic Logic}}

% The pointsizes here depend on the overall pointsize
% and we deal with this by simply taking cases.
% 1998: The results for 12pt will not be very good.
% 1998: and it may be useful to add a third case, if we want a 12pt

\newenvironment{thebibliography}[1]{%
\ifnum\@mainsize=10                 % (at 10pt, the normal for the jsl)
   \par
        \vspace{18pt}%
        \centerline{\fontsize{7}{7\p@}\selectfont \refname}%
        \nobreak\vspace*{5pt}\nobreak%
  \fontsize{8}{10\p@}\selectfont\relax
  \def\and{{\normalfont \lowercase{and}\ }}%
  \list{[\@arabic\c@enumi]\ }{%
        \leftmargin\z@ \labelwidth\z@ \itemindent12\p@
  \labelsep\z@ \usecounter{enumi}}%
  \sloppy \clubpenalty4000\relax \widowpenalty\clubpenalty
  \sfcode`\.\@m
\else                              % (at 11pt or bigger)
  \par
        \vspace{20pt}%
        \centerline{\fontsize{8}{8\p@}\selectfont \refname}%
        \nobreak\vspace*{6pt}\nobreak%
  \fontsize{9}{11\p@}\selectfont\relax
  \def\and{{\normalfont \lowercase{and}\ }}%
  \list{[\@arabic\c@enumi]\ }{%
        \leftmargin\z@ \labelwidth\z@ \itemindent12\p@
  \labelsep\z@ \usecounter{enumi}}%
  \sloppy \clubpenalty4000\relax \widowpenalty\clubpenalty
  \sfcode`\.\@m%
\fi} % end of the pointsize choice
{\endlist}

%-------------------------------------- bibliography options
% \input bibextra.sty
% This (until "endbibextra") is Kapoutsis' bibextra.sty
% implementing the commands required by asl.bst
% It defines the options bibalpha, bibay1 and bibay2
%%%====================================================================
%%%==0== General description ==========================================
%%%====================================================================

% WHAT \bib?item AND \bibxcite DO
%
% Commands \bibfitem, \bibritem replace \bibitem, providing more
% flexibility. Their exact syntax is (see also file asl.bst)
%   \bib?item 1. {citation key}
%             2. {guys-list}
%             3. {year}
%             4. {tag}
%             5. {`number' of editors}
%             6. {hardcite}
%             7. {hardlist}
% Like \bibitem, each one of \bib?item lies in a `thebibliography'
% environment (which is just a special `list' environment) and is
% followed by some text that describes a bibliography entry. The
% only difference between the two commands is that: in a group of
% successive entries that have the same author(s)/editor(s), BibTeX
% will introduce the f-irst entry with \bibfitem and the r-est with
% \bibritem. So, differences in the definitions of the commands may
% produce nice effects. (E.g., see \intergroupsep below.)
%
% As an example, here is a possible case (with any of \bibfitem,
% \bibritem in place of \bib?item):
%   \begin{thebibliography}
%   ...
%   \bib?item{easy}
%   {\guy{C.}{Christos}{}{Kapoutsis}{}}
%   {2020}{a}{0}
%   {}{}
%   \guysmagic{Christos Kapoutsis} {\itshape $\P=\NP$},
%   {\bfseries\itshape Ciao}, vol.\weaktie 30\yearmagic{}{(2020)},
%   no.\weaktie 50, pp.\weaktie 1--2.
%   ...
%   \end{thebibliography}
%
% There are two things the command should do:
%    (i) provide an \item[label] of this `list'-like `thebibliography'
%        environment. The label should be relevant to the corresponding
%        bibliography entry. Obviously, it should be constructed out of
%        the arguments of \bib?item.
%   (ii) write a \bibxcite command to the .aux file. At the beginning
%        of its next pass, LaTeX will execute this command and thus
%        define the citation label for the specific entry (that is,
%        the label that will appear wherever in the text there is a
%        reference to this entry).
%
% [ Note: Below, both commands use \@bib@item as a `submacro' (\bibritem
%   is exactly that; \bibfitem also inserts a vertical space). So, we
%   refer to `the definition of \bib?item', meaning `the definition
%   of \@bib@item'. ]
%
% The first three lines in the definition of \bib?item handle (i):
%    \item[\@wrap@list{%
%       \@ifisempty{#7}{\composelist{#1}{#2}{#3}{#4}{#5}}{#7}%
%    }]%
% This is a typical \item[..] command. (Note that, because of the
% brackets, this does not advance \@listctr, the counter of the list).
% \@wrap@list encloses the result of the second line into brackets (its
% definition is simply that: \def\@wrap@list#1{[#1]}; but, of course,
% it may change; it does, in option bibay2). And the result of the
% second line is
%    -- the hardlist field of the entry (argument #7), if there was
%       such a field in the entry.
%    -- the result of the \composelist macro, otherwise.
%
% It is evident that the syntax of \composelist is
%         \composelist 1. {citation key}
%                      2. {guys-list}
%                      3. {year}
%                      4. {tag}
%                      5. {`number' of editors}
% and that its purpose is to construct a label for the item out of its
% arguments. It is important to note that, being within the environment
% `thebibliography', \composelist has access to the \@listctr counter,
% the counter of the list. Therefore, interesting effects are possible.
% For example, a definition of the form
%    \def\composelist#1#2#3#4#5{%
%        \addtocounter{\@listctr}{1}\the\value{\@listctr}}
% would result in the list label being (almost) the order of the item
% in the bibliography list. (This is the actual definition of the
% command when none of the bibalpha, bibay1, bibay2 options is set.)
%
% [ Note: (To explain `almost'.) Even if \composelist remembers to
%   advance \@listctr, this advance will not be performed at entries
%   that had their hardlist field defined. Indeed, \bib?item will
%   select the value of that field as the label of the entry in the
%   bibliography listing and will not call \composelist at all. ]
%
% The rest of the definition of \bib?item handles (ii). Lines
%    \@auxstring={\bibxcite{#1}{#2}{#3}{#4}{#5}{#6}}
%    \immediate\write\@auxout{\the\@auxstring{\the\value{\@listctr}}}}
% result in printing into the .aux file the command
%    \bibxcite{citation key}{guys-list}{year}{tag}{noe}{hardcite}{n}
% where n = the order of the item in the list (almost). For example,
% assuming the case cited above, the next pass of LaTeX will print
% into the .aux file the command
% \bibxcite{easy}{\guy{C.}{Christos}{}{Kapoutsis}{}}{2020}{a}{0}{}{7}
% if this was (almost) the seventh item in the list...


% CITATION
%
% ...In its next pass, LaTeX will start (as always) by reading the
% .aux file. So, it will execute this \bibxcite command. The result
% will be (see the definition of \bibxcite below) the definition of
% the following macros:
%  \gu@easy -> \guy{C.}{Christos}{}{Kapoutsis}{}
%  \yr@easy -> 2020
%  \yt@easy -> a
%  \cp@easy -> \composecitepre{easy}
%              {\guy{C.}{Christos}{}{Kapoutsis}{}}{2020}{a}{0}{7}
%  \ci@easy -> \composeciteid{easy}
%              {\guy{C.}{Christos}{}{Kapoutsis}{}}{2020}{a}{0}{7}
% (but if there was a nonempty hardcite argument, the \ci@easy macro
% would have been set to that).
%    In the sequel, LaTeX will read the .tex file and (see the
% definition for \citeauth, \citeyear, \citeytag) use \gu@easy to
% replace all \citeauth{easy} references, \yr@easy,\yt@easy for all
% \citeyear{easy} references, etc.
%
% As for the classical \cite command, this is redefined in order
% to serve the following intention: Each citation label consists of
% two parts: `pre' and `id'; `pre' may be empty, but `id' shouldn't.
% For the entry named `easy', the `pre' part is whatever \cp@easy
% has been defined to be and the `id' part is whatever \ci@easy has
% been defined to be. Typically, `pre' will be empty or the list of
% authors' names and `id' will be the year followed by the year tag.
%   [ One can alter these two definitions by properly redefining the
%     \composecitepre and \composeciteid commands. But redefinition
%     of \composeciteid will have no effect if the entry has a
%     hardcite field. See the \bibxcite command below.
%
%     Specifically, the syntax of \composeciteXXX (XXX=pre,id) is
%       \composeciteXXX  1. {citation key}
%                        2. {guys-list}
%                        3. {year}
%                        4. {tag}
%                        5. {`number' of editors}
%                        6. {n}
%     where n is as in the syntax for \bibxcite. The intension is that
%     the command should construct a cite label out of its arguments.
%     E.g.,
%           \def\composecitepre#1#2#3#4#5#6{%
%               \let\oldguy=\guy%
%               \def\guy##1##2##3##4##5{%
%                   ##1\@ifnonempty{##3}{ ##3}~##4}
%               #2%                                                (<-)
%               \global\let\guy=\oldguy}
%     defines the `pre' part to be the list of author(s)/editor(s),
%     in line (<-). The other lines redefine \guy so that a
%     `first-name-initials von last' format is used for the names.
%     So, applied to the running example, this definition would
%     result in this `pre' part of the citation label: `C.~Kapoutsis'.
%   ]
% The `id' part will be always present in a citation, while the `pre'
% part may be gobbled up.
%
% The code `\cite[opt]{easy}' in the text is equivalent to:
%   \@wrap@cite{\@wrap@alone@label{\@make@alone@label{opt}{easy}}}
% Here, \@wrap@cite may provide a wrap for the whole citation, while
% \@wrap@alone@label may provide a wrap for the label. The command
% \@make@alone@label should construct the actual label out of the
% two arguments of \cite and using the \cp@easy, \ci@easy macros
% defined by \bibxcite.
%    The code `\cite{easy}' is equivalent to `\cite[]{easy}'.
%    Infix `alone' implies that the label that is being constructed
% is the only label in the citation.
%
% There may be more than one label, e.g.,
%   \cite{easy,hard}
% where `hard' is the name of some other entry. This code is equivalent
% to this one:
%   \@wrap@cite{%
%       \@wrap@first@label{\@make@first@label{opt}{easy}}%
%       \@wrap@final@label{\@make@final@label{opt}{hard}}%
%   }
% Here, `first' and `final' imply that the corresponding labels appear
% first (and, at the same time, nonlast) and last (and, at the same
% time, nonfirst) in the list, respectively.
%
% There may also be more than two labels. E.g., the command
%   \cite{easy,typical1,..,typicalN,hard}             (for N>0)
% is equivalent to this:
%   \@wrap@cite{%
%       \@wrap@first@label{\@make@first@label{opt}{easy}}%
%       \@wrap@inner@label{\@make@inner@label{opt}{typical1}}%
%       ...
%       \@wrap@inner@label{\@make@inner@label{opt}{typicalN}}%
%       \@wrap@final@label{\@make@final@label{opt}{hard}}%
%   }
% where `inner' implies the construction of a both nonfirst and nonlast
% label.
%
% Properly defining the \@wrap@xxxxx@label commands we can get some
% nice effects. But wrapping also occurs deeper in the construction
% of the label: \@make@xxxxx@label{opt}{easy} is equivalent to
%   \@wrap@xxxxx@label@pre{\@make@label@pre{opt}{easy}}%
%   \@wrap@xxxxx@label@id{\@make@xxxxx@label@id{opt}{easy}}
% which constructs and wraps the `pre' and `id' parts of the label.
%
% The two \@make commands in this level construct the two parts of the
% label in the expected way. The only important thing here is that:
%   in a maximal list of successive labels that have the same `pre'
%   part, \@make@label@pre will return the `pre' part of the first
%   of these labels but just an empty string for every other label.
% This is how, e.g., a list like
%    \cite{Church1949a,Church1949b,Church1950}
% ends up to be
%    Church [1949a], [1949b], [1950]
% and not
%    Church [1949a], Church [1949b], Church [1950]
% in style `bibay2'.
%
% Also note that, in all options, the optional argument `opt' of the
%   \cite[opt]{name1,name2,..,nameN}
% command is appended as a comment to the last entry, that is to the
% antry named `nameN'. But this may also change, by properly redefining
% the \@make@xxxxx@label commands.







%%%====================================================================
%%%==1== General purpose ==============================================
%%%====================================================================

% Some numbers.
\mathchardef\@oh=100
\mathchardef\@fh=500


% To select on the emptiness of an argument. \@ifisempty checks whether
% its first argument is empty. If so, the second argument is selected.
% Otherwise, the third argument is selected.
%
% The trick is to construct a new temporary macro \@iietest as
%    {},  if #1 is empty       or       {blabla},  if #1 is blabla.
% Then, \ifx compares the `top level' expansion of \@iietest and of
% \empty  (whose definition is: \def\empty{}).
\def\@ifisempty#1#2#3{%
    \def\@iietest{#1}%
    \ifx\@iietest\empty #2\else #3\fi}

% To do something only when an argument is nonempty & not `\empty'.
% If #1 is empty, \ifx compares \empty with \else, finds them
%   different and just gobbles up everything up to \fi, doing nothing.
% Otherwise: if #1 is the \empty command, \ifx compares it with
%   \empty, finds them identical, executes everything (nothing) upto
%   the \else command and ignores everything else upto \fi. So, in
%   total, it does nothing at all.
% Otherwise: if #1 is not the \empty command, \ifx compares \empty
%   with (the starting of) #1, finds them different, gobbles up
%   everything up to \else, and executes #2.
% Apparently, #1 shouldn't contain an unmatched \else.
\def\@ifnonempty#1#2{\ifx\empty#1\else#2\fi}

% Redefinition of the \@for#1:=#2\do macro. This macro allows iteration
% over a comma separated list of strings. For example, this code:
%   \@for\temp:= one, two ,   three\do{<\temp>}
% is equivalent to this code:
%   <one><two ><three>
% (Notice that leading spaces are trimmed off, while trailing blanks
% contract to a single space.)
%
% We first copy the set of relevant commands from latex.ltx and then
% we add the lines with the comments on the right. This way, we have
% two new if's:
%  -- \if@sol, which is true iff the current item is the first item
%              of the list, and
%  -- \if@eol, which is true iff the current item is the last item
%              of the list.
\newif\if@sol                             %  !insertion: \@sol created.
\newif\if@eol                             %  !insertion: \@eol created.

\def\@predictor{?}                        % !insertion: updating \@eol.
\def\@end@of@list{\@nil,\@nil}            %
\def\@update@eol#1{%                      %
    \def\@predictor{#1}%                  %
    \ifx\@predictor\@end@of@list%         %
        \@eoltrue%                        %
    \else%                                %
        \@eolfalse%                       %
    \fi%                                  %
}                                         %

\long\def\@for#1:=#2\do#3{%
    \@soltrue%                            %   !insertion: \@sol inited.
    \@eolfalse%                           %   !insertion: \@eol inited.
    \expandafter\def\expandafter\@fortmp\expandafter{#2}%
    \ifx\@fortmp\@empty \else
        \expandafter\@forloop#2,\@nil,\@nil\@@#1{#3}\fi}
\long\def\@forloop#1,#2,#3\@@#4#5{%
    \@update@eol{#2,#3}%                  %  !insertion: \@eol updated.
    \def#4{#1}%
    \ifx #4\@nnil \else%
        #5%
        \@solfalse%                       %  !insertion: \@sol updated.
        \@update@eol{#3}%                 %  !insertion: \@eol updated.
        \def#4{#2}%
        \ifx #4\@nnil \else%
            #5%
            \@iforloop #3\@@#4{#5}%
        \fi%
    \fi%
}
\long\def\@iforloop#1,#2\@@#3#4{%
    \@update@eol{#2}%                     %  !insertion: \@eol updated.
    \def#3{#1}%
    \ifx #3\@nnil%
        \expandafter\@fornoop%
    \else%
        #4\relax%
        \expandafter\@iforloop%
    \fi%
    #2\@@#3{#4}%
}









%%%====================================================================
%%%==2== Material used by all options =================================
%%%====================================================================



%%%==2A. What MUST be defined =========================================
% See asl.bst, Section `COMMANDS THAT THE TEX USER SHOULD DEFINE'.
%     (\bysame, \weaktie, \jslname, \bslname are defined elsewhere.)
% See the discussion above.

\def\yearappear{\def\yearmagic##1##2{##1 ##2}}
\def\yeardisappear{\def\yearmagic##1##2{}}

\def\guysappear{\def\guysmagic##1{##1,}}
\def\guysdisappear{\def\guysmagic##1{}}

\def\guy#1#2#3#4#5{#4}

\def\TheSortKeyIs#1{}

\newtoks\@auxstring
\newlength{\intergroupsep}
\def\@bib@item#1#2#3#4#5#6#7{%
\item[\@wrap@list{%
  \@ifisempty{#7}{\composelist{#1}{#2}{#3}{#4}{#5}}{#7}%
}]%
\if@filesw{%
  \def\protect##1{\string ##1\space}%
  \@auxstring={\bibxcite{#1}{#2}{#3}{#4}{#5}{#6}}
  \immediate\write\@auxout{\the\@auxstring{\the\value{\@listctr}}}}
\fi
\mbox{ }\ignorespaces
}
\def\bibfitem{\vspace{\intergroupsep}\@bib@item}
\let\bibritem=\@bib@item


%%%==2B. Citation enhancements ========================================

% Define all necessary entry data as macros.
\def\bibxcite#1#2#3#4#5#6#7{
  \global\@namedef{gu@#1}{#2}                      % guys list
  \global\@namedef{yr@#1}{#3}                      % year
  \global\@namedef{yt@#1}{#4}                      % year tag
  \global\@namedef{cp@#1}{\composecitepre{#1}{#2}{#3}{#4}{#5}{#7}}  %%%
  \@ifisempty{#6}                                  % cite  pre  &  id %
    {\global\@namedef{ci@#1}{\composeciteid{#1}{#2}{#3}{#4}{#5}{#7}}} %
    {\global\@namedef{ci@#1}{#6}}                                   %%%
}

% Action to be taken when a citation key is used without having been
% defined yet: A bold question mark is left and a warning is printed.
\def\cite@nondef@type{{\reset@font\bf ?}}
\def\cite@nondef@warn#1{%
    \@warning{Citation `#1' on page \thepage\space undefined}}
\def\cite@nondef#1{\cite@nondef@type\cite@nondef@warn{#1}}


% CITATION SPACING
%
% For the construction of the citations we define two spaces: a hard
% one (\@hard@cispace) which does not break at line end and a soft one
% (\@soft@cispace) which breaks. \@cispace is always one of these two
% spaces. We use \@cispace each time we want a space which we do not
% know for sure if it should be hard or soft.
%    When preceding a \cite or \fullcite command, the \XXXXcispacehere
% command (XXXX=hard,soft) makes \@cispace be XXXX for this citation.
%    Command \XXXXcispace makes \@cispace be XXXX for all citations
% after it and before the next \XXXXcispace command.
%    This behavior is achieved through the use of \@global@cispace and
% the reassignment of \@cispace to that, at the end of the \cite
% command definition.
\def\@hard@cispace{\penalty\@m\space}
\def\@soft@cispace{\space}
\def\hardcispacehere{\let\@cispace=\@hard@cispace}
\def\softcispacehere{\let\@cispace=\@soft@cispace}
\def\hardcispace{
    \let\@global@cispace=\@hard@cispace
    \let\@cispace=\@hard@cispace}
\def\softcispace{
    \let\@global@cispace=\@soft@cispace
    \let\@cispace=\@soft@cispace}


% REDEFINITION OF THE \cite MACRO    (See a discussion above.)
%
% Store the old \cite and \@citex 
% and put this in a command
\let\oldcite\cite
\let\old@citex\@citex
\let\oldbibitem\bibitem

% To turn the old style on
\def\oldbib{\let\cite\oldcite\let\@citex\old@citex\bibitemfalse}
\newif\ifbibitem
\bibitemtrue
\def\bibitem{\ifbibitem\errbibitem\bibitemfalse\fi\oldbibitem}

\def\errbibitem{%
\errmessage{**************************************************^^J
There are (ugh!) bibitems in your .bbl file!^^J^^J
Use the option bibother to get the correct citations.^^J^^J
And think about using bibliographystyle{asl}^^J
which is highly recommended for ASL publications!^^J
**************************************************^^J}}



\DeclareRobustCommand\cite{%
    \@ifnextchar [{\@tempswatrue\@citex}{\@tempswafalse\@citex[]}}

\def\@citex[#1]#2{%
%properly wrap the whole list of citations:
    \@wrap@cite{%
%now traverse the list, \@citeb always being the current item.
        \@for\@citeb:=#2\do{%
%gobble one initial space, if present:
            \edef\@citeb{\expandafter\@firstofone\@citeb\@empty}%
%output sth like `\citation{Church36a}' to the auxiliary file:
            \if@filesw%
                \immediate\write\@auxout{\string\citation{\@citeb}}%
            \fi%
%select the proper \@wrap@xxxxx@label, \@make@xxxxx@label commands:
            \if@sol%
                \if@eol%
                    \def\@wrap@label{\@wrap@alone@label}%
                    \def\@make@label{\@make@alone@label}%
                \else%
                    \def\@wrap@label{\@wrap@first@label}%
                    \def\@make@label{\@make@first@label}%
                \fi%
            \else%
                \if@eol%
                    \def\@wrap@label{\@wrap@final@label}%
                    \def\@make@label{\@make@final@label}%
                \else%
                    \def\@wrap@label{\@wrap@inner@label}%
                    \def\@make@label{\@make@inner@label}%
                \fi%
            \fi%
%make this item's label, and print it properly wrapped.
            \@wrap@label{\@make@label{#1}{\@citeb}}%
        }%
    }%
%reset spacing to be as in global definition.
    \let\@cispace=\@global@cispace%
}

% making label parts:
\def\@make@alone@label#1#2{%
    \@wrap@alone@label@pre{\@make@label@pre{#1}{#2}}%
    \@wrap@alone@label@id{\@make@alone@label@id{#1}{#2}}%
}
\def\@make@first@label#1#2{%
    \@wrap@first@label@pre{\@make@label@pre{#1}{#2}}%
    \@wrap@first@label@id{\@make@first@label@id{#1}{#2}}%
}
\def\@make@final@label#1#2{%
    \@wrap@final@label@pre{\@make@label@pre{#1}{#2}}%
    \@wrap@final@label@id{\@make@final@label@id{#1}{#2}}%
}
\def\@make@inner@label#1#2{%
    \@wrap@inner@label@pre{\@make@label@pre{#1}{#2}}%
    \@wrap@inner@label@id{\@make@inner@label@id{#1}{#2}}%
}
\def\@prev@label@pre{}              % \@prev@label@pre:    *init
\def\@make@label@pre#1#2{%
    \@ifundefined{cp@#2}
        {\cite@nondef@type}
        {\edef\@curr@label@pre{\csname cp@#2\endcsname}%
         \ifx\@curr@label@pre\@prev@label@pre\else%
            \@wrap@label@pre{\@curr@label@pre}%
         \fi%
         \global\edef\@prev@label@pre{\@curr@label@pre}%   *update
        }%
}
\def\@make@alone@label@id#1#2{%
    \@ifundefined{ci@#2}
        {\cite@nondef{#2}%
         \@ifnonempty{#1}{,\@cispace#1}%
        }
        {\csname ci@#2\endcsname%
         \@ifnonempty{#1}{,\@cispace#1}%
         \global\def\@prev@label@pre{}%                    *reset
        }%
}
\def\@make@first@label@id#1#2{%
    \@ifundefined{ci@#2}
        {\cite@nondef{#2}}
        {\csname ci@#2\endcsname}%
}
\def\@make@final@label@id#1#2{%
    \@ifundefined{ci@#2}
        {\cite@nondef{#2}%
         \@ifnonempty{#1}{,\@cispace#1}%
        }
        {\csname ci@#2\endcsname%
         \@ifnonempty{#1}{,\@cispace#1}%
         \global\def\@prev@label@pre{}%                    *reset
        }%
}
\def\@make@inner@label@id#1#2{%
    \@ifundefined{ci@#2}
        {\cite@nondef{#2}}
        {\csname ci@#2\endcsname}%
}


% VARIANTS OF THE \cite MACRO
%
% For each variant, the (unique) argument should contain only one
% entry name (that is, a comma-separated list of entry names is not
% supported). Moreover, there is no optional argument.
\def\citeauth#1{%     For the list of author(s).
\@ifundefined{gu@#1}
   {\cite@nondef{#1}}
   {\csname gu@#1\endcsname}}

\def\citeyear#1{%     For the year + year tag label.
\@ifundefined{yr@#1}
   {\cite@nondef{#1}}
   {\csname yr@#1\endcsname\csname yt@#1\endcsname}}

\def\citeytag#1{%     For the year tag.
\@ifundefined{yt@#1}
   {\cite@nondef{#1}}
   {\csname yt@#1\endcsname}}

\DeclareRobustCommand\fullcite{%      For a full-name citation.
    \@ifnextchar [{\@tempswatrue\@fcitex}{\@tempswafalse\@fcitex[]}}
\def\@fcitex[#1]#2{%
    \let\oldguy=\guy%
    \def\guy##1##2##3##4##5{%
        ##2 \@ifnonempty{##3}{##3}~##4\@ifnonempty{##5}{, ##5}}%
    \@citex[#1]{#2}%
    \global\let\guy=\oldguy%
}


% REDEFINITION OF THE \nocite MACRO    (See a discussion above.)
%
% This is almost identical to the definition of the command in
% latex.ltx. The only difference is the first argument of the
% \@ifundefined command, which is `gu@\@citeb' instead of
% `b@\@citeb'.
\def\nocite#1{%
%some initial hacking I don't understand:
    \@bsphack%
%traverse the comma-separated list, \@citeb being the current item:
    \@for\@citeb:=#1\do{%
%gobble one initial space, if present:
        \edef\@citeb{\expandafter\@firstofone\@citeb}%
%output sth like `\citation{Church36a}' to the auxiliary file:
        \if@filesw%
            \immediate\write\@auxout{\string\citation{\@citeb}}%
        \fi%
%check if the citation has been defined previously:
        \@ifundefined{gu@\@citeb}%
            {\G@refundefinedtrue%
             \@latex@warning{Citation `\@citeb' undefined}}%
            {}%
    }%
%some final hacking I don't understand:
  \@esphack%
}
% Also define the control sequence \gu@*, to prevent a warning when
% the \nocite{*} command is used.
\expandafter\let\csname gu@*\endcsname\@empty








%%%====================================================================
%%%==3== The Options ==================================================
%%%====================================================================



%%%==3A. Option None (default style) ==================================
\guysappear
\yearappear
\setlength{\intergroupsep}{0ex}
\def\composelist#1#2#3#4#5{%
    \addtocounter{\@listctr}{1}\the\value{\@listctr}}
\def\@wrap@list#1{[#1]}

\def\composecitepre#1#2#3#4#5#6{}
\def\composeciteid#1#2#3#4#5#6{#6}
\def\@wrap@cite#1{[#1]}
\def\@wrap@alone@label#1{#1}
\def\@wrap@first@label#1{#1,\@cispace}
\def\@wrap@final@label#1{#1}
\def\@wrap@inner@label#1{#1,\@cispace}
\def\@wrap@alone@label@pre#1{#1}
\def\@wrap@first@label@pre#1{#1}
\def\@wrap@final@label@pre#1{#1}
\def\@wrap@inner@label@pre#1{#1}
\def\@wrap@alone@label@id#1{#1}
\def\@wrap@first@label@id#1{#1}
\def\@wrap@final@label@id#1{#1}
\def\@wrap@inner@label@id#1{#1}
\def\@wrap@label@pre#1{#1}
\hardcispace


%%%==3B. Option bibalpha ==============================================
\DeclareOption{bibalpha}{
\guysappear
\yearappear
\def\composelist#1#2#3#4#5{#1}

\def\composecitepre#1#2#3#4#5#6{}
\def\composeciteid#1#2#3#4#5#6{#1}
\def\@wrap@first@label#1{#1,\@cispace}
\def\@wrap@inner@label#1{#1,\@cispace}
}


%%%==3C. Option bibay1 (author-year style) ============================
\DeclareOption{bibay1}{
\guysappear
\yearappear
\def\composelist#1#2#3#4#5{#3#4}
\def\composecitepre#1#2#3#4#5#6{#2}
\def\composeciteid#1#2#3#4#5#6{#3#4}
\def\@wrap@cite#1{#1}
\def\@wrap@first@label#1{#1}
\def\@wrap@inner@label#1{#1}
\def\@wrap@alone@label@id#1{,\@cispace#1]}
\def\@wrap@first@label@id#1{,\@cispace#1}
\def\@wrap@final@label@id#1{,\@cispace#1]}
\def\@wrap@inner@label@id#1{,\@cispace#1}
\def\@wrap@label@pre#1{\if@sol\else],\space\fi[#1}
}


%%%==3D. Option bibay2 (author-year style) ============================
\DeclareOption{bibay2}{
\guysdisappear
\yeardisappear
\def\edlabel{\space(editor)\space}
\def\edslabel{\space(editors)\space}
\def\composelist#1#2#3#4#5{%
    \let\oldguy=\guy%
    \def\guy##1##2##3##4##5{%
        ##2 \@ifnonempty{##3}{##3~}##4\@ifnonempty{##5}{, ##5}}%
    \ifcase#5\textsc{#2}\space\or#2\edlabel\or#2\edslabel\fi[#3#4],%
    \global\let\guy=\oldguy}
\def\@wrap@list#1{#1}

\def\composecitepre#1#2#3#4#5#6{#2}
\def\composeciteid#1#2#3#4#5#6{#3#4}
\def\@wrap@cite#1{#1}
\def\@wrap@first@label#1{#1}
\def\@wrap@inner@label#1{#1}
\def\@wrap@alone@label@id#1{[#1]}
\def\@wrap@first@label@id#1{[#1],\space}
\def\@wrap@final@label@id#1{[#1]}
\def\@wrap@inner@label@id#1{[#1],\space}
\def\@wrap@label@pre#1{#1\@cispace}
}

%%%%%%%%%%% Bib styles other than asl.bst

\DeclareOption{bibother}{\oldbib}

%%%%%%%%%%% endbibextra code

%-------------------------------------- endnotes options
% \input ntsextra.sty
% Until "endntsextra" this is the Kapoutsis customization of
% endnotes.sty
% It defines options endnotes and mixednotes

%%%====================================================================
%%%==1== The options ==================================================
%%%====================================================================

% The standard option is just the file aslnotes.sty. Both options that
% follow (endnotes, mixednotes) first execute the standard one.

%%%==1A. The standard option ==========================================
\DeclareOption{stdnotes}{
% This is file aslnotes.sty, version 1.0, inserted.
% A version of John Lavagnino's endnotes.sty customized for asl.cls
% by Christos Kapoutsis. For the customization, see sections marked
% ckap at end. For instructions search for instructions (at the end).

\message{^^J
Using a version of John Lavagnino's endnotes.sty [1991/09/24]^^J
customized for asl.cls by Christos Kapoutsis^^J^^J}

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%       ****************************************
%       *              ENDNOTES                *
%       ****************************************
%
%  Date of this version: 24 September 1991.
%
%  Based on the FOOTNOTES section of
%  LATEX.TEX (VERSION 2.09 - RELEASE OF 19 April 1986), with
%  "footnote" changed to "endnote" and "fn" changed to "en" (where
%  appropriate), with all the minipage stuff pulled out, and with
%  some small changes for the different operation of endnotes.
%
%  Uses an extra external file, with .ENT extension, to hold the
%  text of the endnotes.  This may be deleted after the run; a new
%  version is generated each time.
%
%  This code does not obey \nofiles.  Perhaps it should.
%
%   John Lavagnino (lav@brandeis.bitnet), 9/23/88
%   Department of English and American Literature,
%      Brandeis University
%
%  To turn all the footnotes in your documents into endnotes, say
%
%     \let\footnote=\endnote
%
%  in your preamble, and then add something like
%
%     \newpage
%     \begingroup
%     \parindent 0pt
%     \parskip 2ex
%     \def\enotesize{\normalsize}
%     \theendnotes
%     \endgroup
%
% as the last thing in your document.
%
%       ****************************************
%       *            CHANGE LOG                *
%       ****************************************
%
% JL  Modified to include \addtoendnotes.  JL, 10/22/89.
%
% JK  Modification by J"org Knappen 25. 2. 1991:
% JK
% JK  Introduced \notesname in the spirit of international \LaTeX.
% JK  \notesname is set per default to be {Notes}, but can easily
% JK  be redifined, e.g. for german language
% JK  \renewcommand{\notesname}{Anmerkungen}
%
% DW Modification by Dominik Wujastyk, London, 19 September 1991:
% DW
% DW Moved the line
% DW         \edef\@currentlabel{\csname p@endnote\endcsname\@theenmark}
% DW out of the definition of \@endnotetext and into the definition
% DW of \@doanenote so that \label and \ref commands work correctly in
% DW endnotes.  Otherwise, the \label just pointed to the last section
% DW heading (or whatever) preceding the \theendnotes command.
%
% JL Revised documentation and macros.  24 Sept 1991.
%
%       ****************************************
%       *        ENDNOTE COMMANDS              *
%       ****************************************
%
%
%   \endnote{NOTE}       : User command to insert a endnote.
%
%   \endnote[NUM]{NOTE}  : User command to insert a endnote numbered
%                           NUM, where NUM is a number -- 1, 2,
%                           etc.  For example, if endnotes are numbered
%                           *, **, etc. within pages, then \endnote[2]{...}
%                           produces endnote '**'.  This command does not
%                           step the endnote counter.
%
%   \endnotemark[NUM]    : Command to produce just the endnote mark in
%                           the text, but no endnote.  With no argument,
%                           it steps the endnote counter before generating
%                           the mark.
%
%   \endnotetext[NUM]{TEXT} : Command to produce the endnote but no
%                              mark.  \endnote is equivalent to
%                              \endnotemark \endnotetext .
%
%   \addtoendnotes{TEXT} : Command to add text or commands to current
%                              endnotes file: for inserting headings,
%                              pagebreaks, and the like into endnotes
%                              sections.  TEXT a moving argument:
%                              \protect required for fragile commands.
%
%       ****************************************
%       *        ENDNOTE USER COMMANDS         *
%       ****************************************
%
%   Endnotes use the following parameters, similar to those relating
%   to footnotes:
%
%   \enotesize   : Size-changing command for endnotes.
%
%   \theendnote : In usual LaTeX style, produces the endnote number.
%
%   \@theenmark : Holds the current endnote's mark--e.g., \dag or '1' or 'a'.
%
%   \@makeenmark : A macro to generate the endnote marker from \@theenmark
%                  The default definition is \hbox{$^\@theenmark$}.
%
%   \@makeentext{NOTE} :
%        Must produce the actual endnote, using \@theenmark as the mark
%        of the endnote and NOTE as the text.  It is called when effectively
%        inside a \parbox, with \hsize = \columnwidth.  For example, it might
%        be as simple as
%               $^{\@theenmark}$ NOTE
%
%
%       ****************************************
%       *        ENDNOTE PSEUDOCODE            *
%       ****************************************
%
% \endnote{NOTE}  ==
%  BEGIN
%    \stepcounter{endnote}
%    \@theenmark :=G eval (\theendnote)
%    \@endnotemark
%    \@endnotetext{NOTE}
%  END
%
% \endnote[NUM]{NOTE} ==
%  BEGIN
%    begingroup
%       counter endnote :=L NUM
%       \@theenmark :=G eval (\theendnote)
%    endgroup
%    \@endnotemark
%    \@endnotetext{NOTE}
%  END
%
% \@endnotetext{NOTE} ==
%  BEGIN
%    write to \@enotes file: "\@doanenote{ENDNOTE MARK}"
%    begingroup
%       \next := NOTE
%       set \newlinechar for \write to \space
%       write to \@enotes file: \meaning\next
%               (that is, "macro:->NOTE)
%    endgroup
%  END
%
% \addtoendnotes{TEXT} ==
%  BEGIN
%    open endnotes file if not already open
%    begingroup
%       let \protect to \string
%       set \newlinechar for \write to \space
%       write TEXT to \@enotes file
%    endgroup
%  END
%
% \endnotemark      ==
%  BEGIN \stepcounter{endnote}
%        \@theenmark :=G eval(\theendnote)
%        \@endnotemark
%  END
%
% \endnotemark[NUM] ==
%   BEGIN
%       begingroup
%         endnote counter :=L NUM
%        \@theenmark :=G eval(\theendnote)
%       endgroup
%       \@endnotemark
%   END
%
% \@endnotemark ==
%   BEGIN
%    \leavevmode
%    IF hmode THEN \@x@sf := \the\spacefactor FI
%    \@makeenmark          % put number in main text
%    IF hmode THEN \spacefactor := \@x@sf FI
%   END
%
% \endnotetext      ==
%    BEGIN \@theenmark :=G eval (\theendnote)
%          \@endnotetext
%    END
%
% \endnotetext[NUM] ==
%    BEGIN begingroup  counter endnote :=L NUM
%                      \@theenmark :=G eval (\theendnote)
%          endgroup
%          \@endnotetext
%    END
%
%       ****************************************
%       *           ENDNOTE MACROS             *
%       ****************************************
%
\@definecounter{endnote}
\def\theendnote{\arabic{endnote}}

% Default definition
\def\@makeenmark{\hbox{$^{\@theenmark}$}}

\newdimen\endnotesep

\def\endnote{\@ifnextchar[{\@xendnote}{\stepcounter
   {endnote}\xdef\@theenmark{\theendnote}\@endnotemark\@endnotetext}}

\def\@xendnote[#1]{\begingroup \c@endnote=#1\relax
   \xdef\@theenmark{\theendnote}\endgroup
   \@endnotemark\@endnotetext}

%  Here begins endnote code that's really different from the footnote
% code of LaTeX.

\let\@doanenote=0
\let\@endanenote=0

\newwrite\@enotes
\newif\if@enotesopen \global\@enotesopenfalse

\def\@openenotes{\immediate\openout\@enotes=\jobname.ent\relax
      \global\@enotesopentrue}

%  The stuff with \next and \meaning is a trick from the TeXbook, 382,
% there intended for setting verbatim text, but here used to avoid
% macro expansion when the footnote text is written.  \next will have
% the entire text of the footnote as one long line, which might well
% overflow limits on output line length; the business with \newlinechar
% makes every space become a newline in the \@enotes file, so that all
% of the lines wind up being quite short.

\long\def\@endnotetext#1{%
     \if@enotesopen \else \@openenotes \fi
     \immediate\write\@enotes{\@doanenote{\@theenmark}}%
     \begingroup
        \def\next{#1}%
        \newlinechar='40
        \immediate\write\@enotes{\meaning\next}%
     \endgroup
     \immediate\write\@enotes{\@endanenote}}

% \addtoendnotes works the way the other endnote macros probably should
% have, requiring the use of \protect for fragile commands.

\long\def\addtoendnotes#1{%
     \if@enotesopen \else \@openenotes \fi
     \begingroup
        \newlinechar='40
        \let\protect\string
        \immediate\write\@enotes{#1}%
     \endgroup}

%  End of unique endnote code

\def\endnotemark{\@ifnextchar[{\@xendnotemark
    }{\stepcounter{endnote}\xdef\@theenmark{\theendnote}\@endnotemark}}

\def\@xendnotemark[#1]{\begingroup \c@endnote #1\relax
   \xdef\@theenmark{\theendnote}\endgroup \@endnotemark}

\def\@endnotemark{\leavevmode\ifhmode
  \edef\@x@sf{\the\spacefactor}\fi \@makeenmark
   \ifhmode\spacefactor\@x@sf\fi\relax}

\def\endnotetext{\@ifnextchar
    [{\@xendnotenext}{\xdef\@theenmark{\theendnote}\@endnotetext}}

\def\@xendnotenext[#1]{\begingroup \c@endnote=#1\relax
   \xdef\@theenmark{\theendnote}\endgroup \@endnotetext}


%  \theendnotes actually prints out the endnotes.

%  The user may want separate endnotes for each chapter, or a big
% block of them at the end of the whole document.  As it stands,
% either will work; you just say \theendnotes wherever you want the
% endnotes so far to be inserted.  However, you must add
% \setcounter{endnote}{0} after that if you want subsequent endnotes
% to start numbering at 1 again.

%  \enoteformat is provided so user can specify some special formatting
% for the endnotes.  It needs to set up the paragraph parameters, start
% the paragraph, and print the label.  The \leavemode stuff is to make
% and undo a dummy paragraph, to get around the games \section*
% plays with paragraph indenting.

%%%%%%changes-----------------------------------------------------ckap-
%%% So that the style is compatible with that of ASL.
%%%
\def\notesname{NOTES}

\def\enoteheading%
{\ifnum\@mainsize=10               % (at 10pt, the normal for the jsl)
 \par
 \vspace{18pt}%
 \centerline{\fontsize{7}{7\p@}\selectfont \notesname}%
 \nobreak\vspace*{5pt}\nobreak%
 \fontsize{8}{10\p@}\selectfont\relax
\else                              % (at 11pt or bigger)
  \par
  \vspace{20pt}%
  \centerline{\fontsize{8}{8\p@}\selectfont \notesname}%
  \nobreak\vspace*{6pt}\nobreak%
  \fontsize{9}{11\p@}\selectfont\relax
\fi
}

\def\enoteformat{\rightskip\z@ \leftskip\z@ %
   \leavevmode\llap{\hbox{$^{\@theenmark}$}}}

\def\enotesize{\footnotesize}

% Restarts counting the endnotes as 1,2,...
\def\startoverendnotes{\setcounter{endnote}{0}}

% The definition of \ETC. is needed only for versions of TeX prior
% to 2.992.  Those versions limited \meaning expansions to 1000
% characters; in 2.992 and beyond there is no limit.  At Brandeis the
% BIGLATEX program changed the code in the token_show procedure of
% TeX to eliminate this problem, but most ``big'' versions of TeX
% will not solve this problem.

\def\theendnotes%
{\immediate\closeout\@enotes \global\@enotesopenfalse
  \begingroup
    \makeatletter
    \def\@doanenote##1##2>{\def\@theenmark{##1}\par\begingroup
        \edef\@currentlabel{\csname p@endnote\endcsname\@theenmark} %DW
        \enoteformat}
    \def\@endanenote{\par\endgroup}%
    \def\ETC.{\errmessage{Some long endnotes will be truncated; %
                            use BIGLATEX to avoid this}%
    \def\ETC.{\relax}}
    \enoteheading
    \enotesize
    \@input{\jobname.ent}%
  \endgroup%
  }
%%%
%%%%%%end changes-------------------------------------------------ckap-

% End of file aslnotes.sty, version 1.0, inserted.
}



%%%==1B. The default (no option) ======================================
\let\endnote=\footnote
\let\endnotemark=\footnotemark
\let\endnotetext=\footnotetext
\let\addtoendnotes=\@gobble
\let\theendnotes=\empty
\let\startoverendnotes=\empty


%%%==1C. Option endnotes ==============================================
\DeclareOption{endnotes}{
\ExecuteOptions{stdnotes}
\let\footnote=\endnote
\setcounter{endnote}{0}
}


%%%==1D. Option mixednotes ============================================
% The footnotes are numbered as 1,2,..,9,1,2,..,9,1,2,.. etc.
% Attention: This way, for all k=1,2,.. : after the 9k'th footnote and
% before the (9k+1)'th footnote the footnote counter is 0.
\DeclareOption{mixednotes}{
\ExecuteOptions{stdnotes}
\newcounter{prevfootnotepage}
\def\thefootnote{%
    \fnsymbol{footnote}%
    \ifnum\value{footnote}>8\setcounter{footnote}{0}\fi}
}

% endntsextra code

%-------------------------------------- Additional commands (mod)

% Add the Greek cap Ypsilon
\def\Ypsilon{\char7}

% In xfonts, \boldsymbol is redefined because the font-coding
% has an error.

% For the game quantifier, requires rotate.sty
\def\game{\rotate[r]{\rotate[r]{$\mathsf{G}$}}}

% For arithmetic subtraction
\newcommand\dotminus{\mathop{\mbox{$-^{\hspace{-.5em}\cdot}\,\,$}}}

% For the boldface \Delta, \Pi,. in descriptive set theory, to
% make them more noticeably different from the lightface

% For the boldface symbols \Pi, \Sigma, \Delta, etc. in DST
\newcommand\tboldsymbol[1]{\underset{\widetilde{}}{\boldsymbol{#1}}}

% To get McCusker to print correctly in the title, headings,
% and Tables of contents, use \author{Guy \Mc Cusker}
\def\Mc{{\sc Mc}}
\def\Mac{{\sc Mac}}
% This preserves the small cap font when the \uppercase command is applied,
% and the c turns out to be a \sc C

\def\cmr#1{{\fontfamily{cmr}\selectfont #1}}   % For single symbols in CMR
% Use \cmr{\pounds} for \pounds

% The \implies symbol comes out wrong with Times, redefine with cmr:

\def\implies{\,\text{\fontfamily{cmr}\fontshape{n}\selectfont\char61%
$\joinrel\Rightarrow\,$}}

% Boldface italics are used often, so we have a command for them:
\def\bfit{\bfseries\itshape}

% LaTeX symbols are available, because latexsym.sty has been loaded.
% These are the following:
% \mho  \Join  \Box  \Diamond  \leadsto  \sqsubset  \sqsupset
% \lhd  \unlhd  \rhd  \unrhd
% Additional symbols from amsmath

% Prints an OK? sign on the margin
\def\ok{\marginpar{\text{OK?}}}

% Use to leave space so lines line up
% This should have the sanme effect as \phantom
% but is left here for compatibility
\newlength{\ypushlen}
\def\ypush#1{\settowidth{\ypushlen}{#1}\hspace*{\ypushlen}}

% \ytext{text1}{text2} puts text2 at the right end of a box
%                      of size text1, in normalshape, roman.
% For handsetting lists, when necessary.
\newlength{\yboxlen}
\def\ytext#1#2{\settowidth{\yboxlen}{#1}%
\makebox[\yboxlen][r]{\normalfont #2}}

% Eqnarray modification, tighter and with a third column
%%% labels work fine, but the \label command must be given
%%% after the tag command
%%% fleqn does not work in align

% to detect \label - \tag error in eqnarray
\newif\iflabelgiven

% Fix the standard label definition
\let\standardlabel\label

\def\labeltagerr{%
\errmessage{^^J
The sequence label{..} 
tag{..}^^J
is not allowed in the eqnarray environment^^J
give tag{..} 
label{..} instead to get right numbering^^J
Hit X to stop and correct the error^^J
}}

% to detect if \tag has been given
% the command \asl@tagfalse is never needed, because of the locality
% of the \@@eqnrc redefinition, which works only for the line 
% in which the tag occurs
\newif\ifasl@tag


% the redone eqnarray environment
\def\eqnarray{%
% Adjust label to record \labelgiven
\def\label##1{\labelgiventrue\standardlabel{##1}}
% keep the old definition of \@currentlabel
% \let\old@currentlabel=\@currentlabel   % 9/14/2000, ynm do not
\let\notag\nonumber      % so notag can be used
%% make the tag definition local to the eqnarray environment
\def\tag##1{%
% \label -\tag error handling
\iflabelgiven\immediate\labeltagerr \fi
\global\asl@tagtrue\gdef\tagsymbol{##1}%
\xdef\@currentlabel{\tagsymbol}
\iftagsleft@\addtocounter{equation}{-1}
\else%
\def\@@eqncr{
\let\reserved@a\relax
    \ifcase\@eqcnt \def\reserved@a{& & &}\or \def\reserved@a{& &}%
     \or \def\reserved@a{&}\else
       \let\reserved@a\@empty
       \@latex@error{Too many columns in eqnarray environment}\@ehc\fi
     \reserved@a \ifasl@tag(\tagsymbol)\else\fi
     \global\@eqnswtrue\global\@eqcnt\z@\cr}\fi}
%%% end of the local tag
   \stepcounter{equation}%
   \def\@currentlabel{\p@equation\theequation}%
  \global\@eqnswtrue
   \m@th
   \global\@eqcnt\z@
   \if@fleqn\else\tabskip\@centering\fi
   \let\\\@eqncr
   $$\everycr{}\halign to\displaywidth\bgroup%
\hskip\@centering$\displaystyle\tabskip\z@skip%
\hskip \leftmargini{##}$\@eqnsel&%
 \global\@eqcnt\@ne\hskip.5\arraycolsep\hfil$\displaystyle{##}$\hfil%
&\hskip .5\arraycolsep$\displaystyle{##}\hskip \leftmargini$\hfil%
&\global\@eqcnt\tw@%
$\displaystyle{##}$\hfil\tabskip\@centering%
&\global\@eqcnt\thr@@\hb@xt@\z@\bgroup\hss##\egroup% 
\gdef\@currentlabel{\p@equation\theequation}%
\labelgivenfalse
\tabskip\z@skip%
\cr%
%% restore the standard def. of \@currentsymbol
% \gdef\@currentlabel{\old@currentlabel} 9/14/00, ynm do not
}

% To place a qedsymbol in the last equation of a proof

\def\proofend{\global\qedneededfalse
\\[-\@back] &&                 % This is the fake entry
\def\@@eqncr{\let\@tempa\relax
    \ifcase\@eqcnt \def\@tempa{& & &}\or \def\@tempa{& &}
      \else \def\@tempa{&}\fi
     \@tempa \if@eqnsw{\qedsymbol}\fi
     \global\@eqnswtrue\global\@eqcnt\z@\cr}}

% endproofeqnarray environment, for compatibility
\@namedef{endproofeqnarray}{\def\@eqncr{\@seqncr}\eqnarray}
\@namedef{endendproofeqnarray}{%
 \proofend\endeqnarray}

\@namedef{endproofeqnarray*}{\def\@eqncr{\nonumber\@seqncr}\eqnarray}
\@namedef{endendproofeqnarray*}{%
\global\proofend\endeqnarray}

% The next command can be used to make the entry not count 
% in the centering, for example
% \begin{eqnarray}
% a &=& b    &\text{comment}\\
% c &=& \forget{very long formula}\\
% e &=& f(y) &\text{comment}
% \end{eqnarray}
% which lines up the comments as they appear to be.
\def\forget#1{\protect\makebox[0pt]{$#1$}}

%-------------------------------------- Heads

% First page has only the page number, but normalsize
\def\ps@firstpage{\ps@plain
\def\@oddfoot{%                                             % jsl-l
\normalfont\fontsize{8}{10\p@}\selectfont \hfil\thepage\hfil
     \global\topskip\normaltopskip}%
  \let\@evenfoot\@oddfoot
  \def\@oddhead{\@serieslogo\hss}%
  \let\@evenhead\@oddhead % in case an article starts on a left-hand page
}

\def\ps@headings{\ps@empty
 \def\@evenhead{\normalfont\fontsize{10}{13\p@}\selectfont
    \rlap{\thepage}\hfil
\fontsize{7}{8\p@}\selectfont\leftmark{}{} \hfil}%
 \def\@oddhead{\normalfont\fontsize{7}{8\p@}\selectfont
    \hfil\rightmark{}{} \hfil
    \fontsize{10}{13\p@}\selectfont \llap{\thepage}}%
  \let\@mkboth\markboth
}

\pagestyle{headings}    % Initialize again

%%%%%%%%%%%%%%%% For the production version, aslprod.sty

% Definition of commands redefined in aslprod.sty

\def\typesetter#1{}
\def\papertype#1{}
\def\revauthor#1{}
\def\revdate#1{}

\def\@location{}   % There is also a change in the definition of \location


\InputIfFileExists{aslprod.sty}{}

%------------------------------- End of asl.sty code

%******************** Font Definitions for Monotype Times *************%

%-------------------------------- OT1 Definitions
\DeclareFontFamily{OT1}{xgt}{}

\DeclareFontShape{OT1}{xgt}{b}{n}{
   <-> gamatb
}{}

\DeclareFontShape{OT1}{xgt}{b}{sc}{
   <-> sub * xgt/b/n                    % corr. 1/16/2000, ynm
}{}

\DeclareFontShape{OT1}{xgt}{b}{sl}{<->ssub * xgt/b/it}{}

\DeclareFontShape{OT1}{xgt}{b}{it}{
   <-> gamatbti
}{}

\DeclareFontShape{OT1}{xgt}{m}{n}{
   <-> gamatr
}{}

\DeclareFontShape{OT1}{xgt}{m}{sc}{
   <-> gamatcsc
}{}

\DeclareFontShape{OT1}{xgt}{m}{sl}{<->ssub * xgt/m/it}{}

\DeclareFontShape{OT1}{xgt}{m}{it}{
   <-> gamatti
}{}

%% Fontshape scr gives true script.

\DeclareFontShape{OT1}{xgt}{m}{scr}{
   <-> gamascr
}{}

%% Fontshape th sets in italics but with Roman punctuation.

\DeclareFontShape{OT1}{xgt}{m}{th}{
   <-> gamatth
}{}

\DeclareFontShape{OT1}{xgt}{bx}{n}{<->ssub * xgt/b/n}{}
\DeclareFontShape{OT1}{xgt}{bx}{sc}{<->ssub * xgt/b/sc}{}
\DeclareFontShape{OT1}{xgt}{bx}{sl}{<->ssub * xgt/b/it}{}
\DeclareFontShape{OT1}{xgt}{bx}{it}{<->ssub * xgt/b/it}{}
\DeclareFontShape{OT1}{xgt}{l}{n}{<->ssub * xgt/m/n}{}
\DeclareFontShape{OT1}{xgt}{l}{sc}{<->ssub * xgt/m/sc}{}
\DeclareFontShape{OT1}{xgt}{l}{sl}{<->ssub * xgt/m/sl}{}
\DeclareFontShape{OT1}{xgt}{l}{it}{<->ssub * xgt/m/it}{}
\DeclareFontShape{OT1}{xgt}{bx}{th}{<->ssub * xgt/b/it}{} % for bold in thms.


%------------------------------------- OMS Definitions

\DeclareFontFamily{OMS}{xgt}{\skewchar\font48 }
\DeclareFontShape{OMS}{xgt}{m}{n}
   {<-> ssub * cmsy/m/n}{}
\DeclareFontShape{OMS}{xgt}{m}{it}
   {<-> ssub * cmsy/m/n}{}
\DeclareFontShape{OMS}{xgt}{m}{sl}
   {<-> ssub * cmsy/m/n}{}
\DeclareFontShape{OMS}{xgt}{m}{sc}
   {<-> ssub * cmsy/m/n}{}
\DeclareFontShape{OMS}{xgt}{bx}{n}
   {<-> ssub * cmsy/b/n}{}
\DeclareFontShape{OMS}{xgt}{bx}{it}
   {<-> ssub * cmsy/b/n}{}
\DeclareFontShape{OMS}{xgt}{bx}{sl}
   {<-> ssub * cmsy/b/n}{}
\DeclareFontShape{OMS}{xgt}{bx}{sc}
   {<-> ssub * cmsy/b/n}{}

%------------------------------------------ OML Definitions

\DeclareFontFamily{OML}{xgt}{\skewchar\font127 }

\DeclareFontShape{OML}{xgt}{m}{it}{
      <-> gamatmi
      }{}
\DeclareFontShape{OML}{xgt}{b}{it}{%
      <-> gamatmib
      }{}

% This is the true script

\DeclareFontShape{OML}{xgt}{m}{scr}{
   <-> gamascr
}{}

\DeclareFontShape{OML}{xgt}{bx}{it}
   {<-> ssub * xgt/b/it}{}

%% Includes psamsfonts and all corrections needed for the psfonts

\InputIfFileExists{amsbug.sty}{}


%--------------------------------------------
\ExecuteOptions{leqno}

\ProcessOptions\relax
\RequirePackage{latexsym}

% Load amssymb.sty (which loads amsfonts.sty) if they exist.
\def\amssymbtest{1}
\IfFileExists{amssymb.sty}{\gdef\amssymbtest{0}}%
{\GenericWarning{}{WARNING:^^JPackage amssymb.sty not found
^^Jso AMS fonts not loaded.}}

\if\amssymbtest 0\let\mathfrak\relax\RequirePackage{amssymb}\fi

% Do not allow compatibility mode
%\if@compatibility \else\endinput\fi
%\def\tiny{\Tiny}
%\def\defaultfont{\normalfont}
%\def\rom{\textup}
\endinput
%%
%% End of file `asl.cls'.
