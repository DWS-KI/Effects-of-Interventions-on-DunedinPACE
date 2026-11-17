-- Run AFTER Quarto so numbers and link targets come from its cross-reference
-- resolver. Keep every @key in the manuscript; never infer numbers from source
-- order or turn an author-written list into a pair of endpoint citations.
-- See README.md for supported syntax and output formats.

local kinds = {
  fig = {singular = "Figure", plural = "Figures", prefix = ""},
  tbl = {singular = "Table", plural = "Tables", prefix = ""},
  suppfig = {singular = "Figure", plural = "Figures", prefix = "S"},
  supptbl = {singular = "Table", plural = "Tables", prefix = "S"},
}

local function reference(inline)
  if inline.t ~= "Link" or not inline.classes:includes("quarto-xref") then
    return nil
  end
  local kind = inline.target:match("^#([a-z]+)%-")
  local style = kinds[kind]
  if not style then return nil end

  -- Match only this manuscript's ordinary, integer-numbered references.
  -- Leave custom prefixes, subfigures and chapter numbers untouched.
  local label = pandoc.utils.stringify(inline.content):gsub(" ", " ")
  local number = label:match("^" .. style.singular .. "%s+" ..
                            style.prefix .. "([1-9]%d*)$")
  if not number then return nil end
  return {kind = kind, number = tonumber(number), link = inline}
end

local function is_space(inline)
  return inline and (inline.t == "Space" or inline.t == "SoftBreak")
end

local function next_reference(inlines, index)
  while is_space(inlines[index]) do index = index + 1 end
  local delimiter = inlines[index]
  if not delimiter or delimiter.t ~= "Str" or
      (delimiter.text ~= "," and delimiter.text ~= ";") then
    return nil
  end
  index = index + 1
  while is_space(inlines[index]) do index = index + 1 end
  if not inlines[index] then return nil end
  local ref = reference(inlines[index])
  if ref then return ref, index end
end

local function number_link(ref, prefix)
  return pandoc.Link({pandoc.Str(prefix .. tostring(ref.number))},
                     ref.link.target, ref.link.title, ref.link.attr)
end

local function compact(references)
  local groups, order, seen = {}, {}, {}
  local has_multiple = false
  for _, ref in ipairs(references) do
    if not groups[ref.kind] then
      groups[ref.kind] = {}
      order[#order + 1] = ref.kind
    end
    if not seen[ref.link.target] then
      local group = groups[ref.kind]
      group[#group + 1] = ref
      if #group > 1 then has_multiple = true end
      seen[ref.link.target] = true
    end
  end
  if not has_multiple then return nil end

  local result = pandoc.Inlines({})
  for group_index, kind in ipairs(order) do
    local group, style = groups[kind], kinds[kind]
    table.sort(group, function(a, b) return a.number < b.number end)
    -- Ambiguous numbering should retain the original references, not hide one.
    for i = 2, #group do
      if group[i].number == group[i - 1].number then return nil end
    end
    if group_index > 1 then
      result:extend({pandoc.Str(";"), pandoc.Space()})
    end
    if #group == 1 then
      result:insert(group[1].link)
    else
      result:extend({pandoc.Str(style.plural), pandoc.Space()})
      local first = 1
      while first <= #group do
        local last = first
        while last < #group and group[last + 1].number == group[last].number + 1 do
          last = last + 1
        end
        if first > 1 then
          result:extend({pandoc.Str(","), pandoc.Space()})
        end
        result:insert(number_link(group[first], style.prefix))
        if last > first then
          result:insert(pandoc.Str("–"))
          result:insert(number_link(group[last], ""))
        end
        first = last + 1
      end
    end
  end
  return result
end

local function compact_paragraph(block)
  local inlines, result, index = block.content, pandoc.Inlines({}), 1
  while index <= #inlines do
    local first = reference(inlines[index])
    local replacement, last
    if first then
      local refs = {first}
      last = index
      while true do
        local ref, position = next_reference(inlines, last + 1)
        if not ref then break end
        refs[#refs + 1], last = ref, position
      end
      if #refs > 1 then replacement = compact(refs) end
    end
    if replacement then
      result:extend(replacement)
      index = last + 1
    else
      result:insert(inlines[index])
      index = index + 1
    end
  end
  block.content = result
  return block
end

-- Headings retain their complete links and labels: the supplementary Excel
-- exporter reads them to obtain the authoritative table numbers.
return {{Para = compact_paragraph, Plain = compact_paragraph}}
