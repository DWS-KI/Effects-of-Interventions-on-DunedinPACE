# Supplementary table workbook

The normal local project render of `Manuscript.qmd` exports 17 supplementary
tables, including the extreme weights sensitivity analysis, into one Excel workbook
under `Output/supplementary_tables/`. The self-reported conditions and concurrent
interventions tables remain in the manuscript.
All supplementary figure sections remain in the manuscript.

Install `openxlsx` and `xml2` in the R environment used for rendering before using
this export. The hooks check for these dependencies and do not install them.
Existing analysis outputs, including the signed Love-plot `.rds` files, must already
be present. The export never starts an analysis pipeline.

The manuscript retains the original Quarto table identifiers and short numbered
workbook references. Full titles and captions are maintained in
`Program/References/supplementary_table_captions.md`; existing inline R expressions
in these captions are evaluated in the manuscript environment. Edit that file to
update an exported table's title or caption.

After rendering, the export reads Quarto's resolved table references from
`Manuscript.docx`. Each worksheet is named `Table S<number>`. The workbook filename
compresses only consecutive numbers: with the current table order, it is
`Supplementary_Tables_S1,S4-19.xlsx`. The extreme weights sensitivity analysis is
worksheet `Table S8`, between `Table S7` and `Table S9`. Table numbers are never
counted separately in R.

The registered data frames contain the final display values. The extreme weights
table uses the manuscript's shared intervention display names and is sorted by the
numeric standard-IPW estimate, lowest to highest, before percentage formatting.
Its displayed column headings, title, and full caption retain their original wording.
The registered values, columns, row order, signed SMD strings, percentages,
and confidence intervals are written directly to Excel. Styling
changes widths and wrapping only. A read-back check compares the exported table
cells with the registered values before the workbook is published. Excel's blank
cells represent both empty strings and missing values, and integer/double storage
distinctions are ignored during this comparison.

Use the normal DOCX render with project hooks and code execution enabled. The
pre-render hook clears stale intermediate data; the post-render hook rejects
missing or mismatched tables and unresolved or duplicate numbers. A failed render
or export leaves the previous completed workbook in place. A successful export
replaces only the previously recorded generated workbook, so changed numbering
does not accumulate multiple final workbooks. `export_manifest.rds` records the
current filename and table references; only the `.xlsx` file is for submission.

An existing workbook does not update until the manuscript is rendered again.
The current hook targets the default output name, `Manuscript.docx`.
