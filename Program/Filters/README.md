# Compact figure and table references

`compact-crossrefs.lua` runs after Quarto's built-in filters, as configured in
`Manuscript.qmd`. Continue writing **every citation key** in each comma-separated
list, for example:

```markdown
(@supptbl-subgroup-men, @supptbl-subgroup-women, @supptbl-subgroup-younger, @supptbl-subgroup-older, @supptbl-subgroup-european, @supptbl-subgroup-noneuropean)
```

With the current numbering, this displays as `(Tables S14–19)`. The filter reads
the numbers assigned by Quarto on each render, sorts the cited references within
each kind, and compresses only consecutive numbers. If an uncited table is
inserted between cited tables, the display splits into separate ranges or
individual numbers, such as `(Tables S14–16, S18–20)`. It never assumes an
uncited table belongs to the range. Mixed lists are grouped by reference kind,
in order of first appearance, such as `(Figures S16–18; Tables S14–19)`.

Semicolon-separated lists, including Quarto's `[@key; @key]` syntax, also work.
References separated by prose (including `and` or qualifiers) are left alone
to preserve their meaning and order. Single references and headings are
unchanged, including the headings used for supplementary Excel export.

This filter supports the manuscript's `fig`, `tbl`, `suppfig` and `supptbl`
references in DOCX and HTML output with hyperlinks enabled. It preserves link
targets and attributes for each displayed number; a range has links at its two
endpoints, while its intermediate keys remain in the source. Other numbering
styles and output formats without resolved numeric cross-reference links retain
Quarto's normal formatting.
