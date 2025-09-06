-- macros.lua (UTF-8)
-- Replace lightweight TeX macros inside math for non-LaTeX outputs (HTML/EPUB).

-- DEBUG: leave this line to confirm the filter is loaded (you'll see it in the render log)
io.stderr:write("[macros.lua] filter loaded\n")

-- Only modify non-LaTeX outputs; let LaTeX handle macros in PDF
local is_latex = FORMAT:match("latex")

-- Map of TeX macros -> Unicode math equivalents for web-friendly output
-- You can add more mappings here (e.g., ["\\bx"]="𝐱")
local MAP = {
  ["\\by"] = "\\mathbf{y}",
  ["\\bx"] = "\\mathbf{x}",
  ["\\bz"] = "\\mathbf{z}",
  ["\\bbeta"] = "\\boldsymbol{\\beta}",
  ["\\bgamma"] = "\\boldsymbol{\\gamma}",
  ["\\btheta"] = "\\boldsymbol{\\theta}",
}

local function replace_macros(s)
  for k, v in pairs(MAP) do
    -- k is a Lua pattern; backslashes are already escaped, so "\\by" matches \by
    s = s:gsub(k, v)
  end
  return s
end

-- Replace inside math nodes ($...$, \[...\], equation/align, etc.)
function Math(el)
  if is_latex then return nil end
  el.text = replace_macros(el.text)
  return el
end

-- Also replace in raw TeX blocks/inlines (e.g., \begin{align} ... \end{align})
function RawBlock(el)
  if is_latex then return nil end
  if el.format == "tex" or el.format == "latex" then
    el.text = replace_macros(el.text)
    return el
  end
end

function RawInline(el)
  if is_latex then return nil end
  if el.format == "tex" or el.format == "latex" then
    el.text = replace_macros(el.text)
    return el
  end
end
