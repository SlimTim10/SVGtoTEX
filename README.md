# SVGtoTEX

## Install

`stack install`

## Usage

```
svg_to_tex.exe FILENAME (-d|--directory INPUT-DIRECTORY) (-o|--output OUTPUT-DIRECTORY)
```

- `FILENAME` is the target SVG file (with or without .svg extension).
- `INPUT-DIRECTORY` is the input directory path (where the target SVG file is located).
- `OUTPUT-DIRECTORY` is the output directory path (where .pdf and .pdf_tex will be created).

## LaTeX example

```
\usepackage{import}

\newcommand{\executeiffilenewer}[3]{%
  \ifnum\pdfstrcmp%
      {\pdffilemoddate{#1}}%
      {\pdffilemoddate{#2}}%
      >0%
      {\immediate\write18{#3}}%
      \fi%
}
\newcommand{\includesvg}[2][\textwidth]{%
  \def\svgwidth{#1}
  \executeiffilenewer{./Graphics/#2.svg}{./tmp/#2.pdf}%
                     {svg_to_tex #2 -d "./Graphics" -o "./tmp"}%
                     \subimport{./tmp/}{#2.pdf_tex}%
}
```