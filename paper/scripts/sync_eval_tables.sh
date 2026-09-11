#!/usr/bin/env bash
set -euo pipefail

if [[ $# -lt 2 || $# -gt 3 ]]; then
  echo "Usage: $0 <performance_table.tex> <per_program_output_dir> [summary_output_file]" >&2
  exit 1
fi

src="$1"
out_dir="$2"
summary_out_file="${3:-}"
runtime_out_dir="${out_dir}_runtime"
papi_out_dir="${out_dir}_papi"
papi_main_out_dir="${out_dir}_papi_main"
script_dir="$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")" && pwd)"
papi_seed_dir="${script_dir}/../tables/per_program_papi_seed"

if [[ ! -f "$src" ]]; then
  echo "Error: source table file not found: $src" >&2
  exit 1
fi

has_table_begin() {
  local file="$1"
  awk '
    /^\\begin\{table\*?\}/ {found = 1; exit}
    END {exit !found}
  ' "$file"
}

has_tabular_block() {
  local file="$1"
  awk '
    /^\\begin\{tabular\}/ {found = 1; exit}
    END {exit !found}
  ' "$file"
}

normalize_eval_table_output() {
  local file="$1"
  perl -0777 -i -pe '
    s/^\\(?:small|scriptsize|footnotesize)$/\\footnotesize/mg;
    s/\\resizebox\{\\columnwidth\}\{!\}\{%\s*\n//g;
    s/\n\\begin\{adjustbox\}\{max width=\\columnwidth\}\s*\n/\n/g;
    s/\n\\end\{adjustbox\}\s*\n/\n/g;
    s/(\\end\{tabular\})%\s*\n\}/$1/g;
  ' "$file"
}

set_tabcolsep() {
  local file="$1"
  local value="$2"
  perl -0777 -i -pe "
    if (/^\\\\setlength\\{\\\\tabcolsep\\}\\{[^}]+\\}\$/m) {
      s/^\\\\setlength\\{\\\\tabcolsep\\}\\{[^}]+\\}\$/\\\\setlength{\\\\tabcolsep}{$value}/mg;
    } elsif (/^\\\\renewcommand\\{\\\\arraystretch\\}\\{[^}]+\\}\$/m) {
      s/^(\\\\renewcommand\\{\\\\arraystretch\\}\\{[^}]+\\}\n)/\$1\\\\setlength{\\\\tabcolsep}{$value}\n/m;
    } else {
      s/^(\\\\(?:small|footnotesize)\n)/\$1\\\\setlength{\\\\tabcolsep}{$value}\n/m;
    }
  " "$file"
}

rename_runtime_symbols() {
  local file="$1"
  perl -0777 -i -pe '
    s#\\textbf\{Am\}#\\textbf{\$\\mathcal{R}_{gm}\$}#g;
    s#\\textbf\{Ai\}#\\textbf{\$\\mathcal{R}_{gi}\$}#g;
    s#\\textbf\{Sm\}#\\textbf{\$\\mathcal{R}_{f}\$}#g;
    s#\\textbf\{Ai/Am\}#\\textbf{\$\\mathcal{S}_{gm}\$}#g;
    s#\\textbf\{Am/Sm\}#\\textbf{\$\\mathcal{S}_{fo}\$}#g;
    s#\\textbf\{Ai/Sm\}#\\textbf{\$\\mathcal{S}_{fb}\$}#g;
    s#\\makecell\[c\]\{Ai/\\\\Am\}#\\makecell[c]{\$\\mathcal{S}_{gm}\$}#g;
    s#\\makecell\[c\]\{Am/\\\\Sm\}#\\makecell[c]{\$\\mathcal{S}_{fo}\$}#g;
    s#\\makecell\[c\]\{Ai/\\\\Sm\}#\\makecell[c]{\$\\mathcal{S}_{fb}\$}#g;
  ' "$file"
}

set_table_environment() {
  local file="$1"
  local env="$2"
  local pos="$3"
  perl -0777 -i -pe "s/^\\\\begin\\{table\\*?\\}(?:\\[[^]]*\\])?/\\\\begin{$env}[$pos]/m; s/^\\\\end\\{table\\*?\\}\$/\\\\end{$env}/m;" "$file"
}

shrink_column_headers() {
  local file="$1"
  perl -0777 -i -pe '
    s#(\\toprule\n)(.*?)(\n\\midrule)#
      my ($start, $hdr, $end) = ($1, $2, $3);
      my @lines = split(/\n/, $hdr, -1);
      for my $line (@lines) {
        next if $line =~ /^\s*$/;
        next if $line =~ /^\\cmidrule/;
        $line = "\\scriptsize " . $line unless $line =~ /^\\scriptsize\b/;
      }
      $start . join("\n", @lines) . $end
    #se;
  ' "$file"
}

format_summary_layout() {
  local file="$1"
  perl -0777 -i -pe '
    s/^\\begin\{tabular\}\{.*$/\\begin{tabular}{>{\\raggedright\\arraybackslash}p{1.2cm} c c >{\\raggedleft\\arraybackslash}p{0.58cm} >{\\raggedleft\\arraybackslash}p{0.58cm} >{\\raggedleft\\arraybackslash}p{0.58cm} >{\\raggedleft\\arraybackslash}p{0.82cm} >{\\raggedleft\\arraybackslash}p{0.82cm} >{\\raggedleft\\arraybackslash}p{0.58cm} >{\\raggedleft\\arraybackslash}p{0.58cm} >{\\raggedleft\\arraybackslash}p{0.58cm} >{\\raggedleft\\arraybackslash}p{0.82cm} >{\\raggedleft\\arraybackslash}p{0.82cm} >{\\raggedleft\\arraybackslash}p{0.58cm} >{\\raggedleft\\arraybackslash}p{0.58cm} >{\\raggedleft\\arraybackslash}p{0.58cm} >{\\raggedleft\\arraybackslash}p{0.82cm} >{\\raggedleft\\arraybackslash}p{0.82cm}}/m;
    s#\\textbf\{ADT\}#\\textbf{Fd}#g;
    s#\\textbf\{SoA\}#\\textbf{B}#g;
    s#\\textbf\{Buffers\}#\\textbf{B}#g;
    s#& fields & bufs & Am \(s\) & Ai \(s\) & Sm \(s\) & Am/Sm & Ai/Sm & Am \(s\) & Ai \(s\) & Sm \(s\) & Am/Sm & Ai/Sm & Am \(s\) & Ai \(s\) & Sm \(s\) & Am/Sm & Ai/Sm \\\\#
     &  &  & \\makecell[c]{\$\\mathcal{R}_{gm}\$} & \\makecell[c]{\$\\mathcal{R}_{gi}\$} & \\makecell[c]{\$\\mathcal{R}_{f}\$} & \\makecell[c]{\$\\mathcal{S}_{fo}\$} & \\makecell[c]{\$\\mathcal{S}_{fb}\$} & \\makecell[c]{\$\\mathcal{R}_{gm}\$} & \\makecell[c]{\$\\mathcal{R}_{gi}\$} & \\makecell[c]{\$\\mathcal{R}_{f}\$} & \\makecell[c]{\$\\mathcal{S}_{fo}\$} & \\makecell[c]{\$\\mathcal{S}_{fb}\$} & \\makecell[c]{\$\\mathcal{R}_{gm}\$} & \\makecell[c]{\$\\mathcal{R}_{gi}\$} & \\makecell[c]{\$\\mathcal{R}_{f}\$} & \\makecell[c]{\$\\mathcal{S}_{fo}\$} & \\makecell[c]{\$\\mathcal{S}_{fb}\$} \\\\#;
  ' "$file"
}

format_papi_layout() {
  local file="$1"
  local pass_width
  local value_width
  pass_width="$(
    awk '
      BEGIN { in_body = 0; max_len = 0 }
      /^\\midrule$/ { in_body = 1; next }
      /^\\bottomrule$/ { in_body = 0 }
      in_body && / & / {
        name = $0
        sub(/[[:space:]]*&.*/, "", name)
        gsub(/\\_/, "_", name)
        gsub(/[{}]/, "", name)
        len = length(name)
        if (len > max_len) max_len = len
      }
      END {
        width = 0.12 * max_len + 0.20
        if (width < 2.15) width = 2.15
        if (width > 4.00) width = 4.00
        printf "%.2fcm", width
      }
    ' "$file"
  )"
  value_width="$(
    awk '
      BEGIN { in_body = 0; max_len = 0 }
      /^\\midrule$/ { in_body = 1; next }
      /^\\bottomrule$/ { in_body = 0 }
      in_body && / & / {
        line = $0
        sub(/\\\\.*/, "", line)
        n = split(line, parts, /[[:space:]]*&[[:space:]]*/)
        for (i = 3; i <= n; i++) {
          cell = parts[i]
          gsub(/\\textbf\{/, "", cell)
          gsub(/\\allowbreak\{\}/, "", cell)
          gsub(/[{}]/, "", cell)
          gsub(/[[:space:]]+/, "", cell)
          len = length(cell)
          if (len > max_len) max_len = len
        }
      }
      END {
        width = 0.10 * max_len + 0.25
        if (width < 1.18) width = 1.18
        if (width > 1.55) width = 1.55
        printf "%.2fcm", width
      }
    ' "$file"
  )"
  PAPI_PASS_WIDTH="$pass_width" PAPI_VALUE_WIDTH="$value_width" PAPI_METRIC_COLS="7" perl -0777 -i -pe '
    my $w = $ENV{PAPI_PASS_WIDTH};
    my $vw = $ENV{PAPI_VALUE_WIDTH};
    my $cols = $ENV{PAPI_METRIC_COLS};
    my $spec = "\\begin{tabular}{>{\\raggedright\\arraybackslash}p{$w} c *{$cols}{>{\\raggedleft\\arraybackslash}p{$vw}}}";
    s/\\begin\{tabular\}\{[^}]+\}/$spec/g;
    s#/\\allowbreak\{\}#/#g;
  ' "$file"
}

compute_appendix_papi_pass_width() {
  local file="$1"
  awk '
    BEGIN { in_body = 0; max_len = 0 }
    /^\\midrule$/ { in_body = 1; next }
    /^\\bottomrule$/ { in_body = 0 }
    in_body && / & / {
      line = $0
      sub(/\s*\\\\.*/, "", line)
      n = split(line, parts, /[[:space:]]*&[[:space:]]*/)
      name = parts[1]
      gsub(/^[[:space:]]+|[[:space:]]+$/, "", name)
      if (name == "") next
      gsub(/\\_/, "_", name)
      gsub(/[{}]/, "", name)
      len = length(name)
      if (len > max_len) max_len = len
    }
    END {
      width = 0.13 * max_len + 0.30
      if (width < 2.30) width = 2.30
      if (width > 3.60) width = 3.60
      printf "%.2fcm", width
    }
  ' "$file"
}

format_appendix_papi_subrow_layout() {
  local file="$1"
  local pass_width="${2:-$(compute_appendix_papi_pass_width "$file")}"
  APPENDIX_PAPI_PASS_WIDTH="$pass_width" perl -0777 -i -pe '
    my $w = $ENV{APPENDIX_PAPI_PASS_WIDTH};
    my $spec = "\\begin{tabular}{>{\\raggedright\\arraybackslash}p{$w} c *{7}{r}}";
    s#\\begin\{tabular\}\{.*?\}\n\\toprule#$spec\n\\toprule#sg;
    s/^\\setlength\{\\tabcolsep\}\{[^}]+\}$/\\setlength{\\tabcolsep}{1.5pt}/mg;
    s/^\\renewcommand\{\\arraystretch\}\{[^}]+\}$/\\renewcommand{\\arraystretch}{0.98}/mg;
  ' "$file"
}

expand_appendix_papi_pairs_to_subrows() {
  local file="$1"
  local variant_hint="${2:-}"
  local pass_width
  pass_width="$(compute_appendix_papi_pass_width "$file")"
  APPENDIX_PAPI_VARIANT="$variant_hint" perl -0777 -i -pe '
    my $hint = $ENV{APPENDIX_PAPI_VARIANT} // "";
    s#\\caption\{Per-pass PAPI counters for \\texttt\{([^}]*)\}(?: \(([^)]*)\))?[^}]*\}#do {
      my ($prog, $variant) = ($1, defined($2) && length($2) ? $2 : $hint);
      my $row_text =
        ($variant eq "Ai/Si")
          ? "\\(\\mathcal{C}_{gi}\\) and \\(\\mathcal{C}_{fi}\\) denote Gibbon-flat immutable and factored immutable counters beneath each pass"
          : "\\(\\mathcal{C}_{gm}\\) and \\(\\mathcal{C}_{f}\\) denote Gibbon-flat mutable and factored mutable counters beneath each pass";
      "\\caption{Per-pass PAPI counters for \\texttt{$prog}. $row_text. CYC=CPU cycles; L1D/L1I/L2D/L2I=cache misses; LLC=LLC load misses.}";
    }#egs;
  ' "$file"
  perl -i -pe '
    BEGIN { $in_body = 0; }
    if (/^\\midrule$/) {
      $in_body = 1;
    } elsif (/^\\bottomrule$/) {
      $in_body = 0;
    } elsif ($in_body && / & /) {
      my $line = $_;
      my $row_end = "";
      $line =~ s/\s*\\\\\s*$// and $row_end = " \\\\";
      my @parts = split(/\s*&\s*/, $line, -1);
      if (@parts >= 9) {
        my ($pass, $type, @metrics) = @parts;
        my (@row_a, @row_s);
        my ($lhs_label, $rhs_label) =
          ($variant eq "Ai/Si")
            ? ("\\mathcal{C}_{gi}", "\\mathcal{C}_{fi}")
            : ("\\mathcal{C}_{gm}", "\\mathcal{C}_{f}");
        for my $cell (@metrics) {
          $cell =~ s/^\s+//;
          $cell =~ s/\s+$//;
          my ($lhs, $rhs) = split(m#/#, $cell, 2);
          if (defined $rhs) {
            $lhs =~ s/\s+$//;
            $rhs =~ s/^\s+//;
          } else {
            $lhs = $cell;
            $rhs = $cell;
          }
          push @row_a, $lhs;
          push @row_s, $rhs;
        }
        my @blank_metrics = ("") x scalar(@metrics);
        my $pass_line = join(" & ", $pass, $type, @blank_metrics) . $row_end . "\n";
        my $a_line = join(" & ", "\\hspace*{1.2em}{\\scriptsize\\($lhs_label\\)}", "", @row_a) . $row_end . "\n";
        my $s_line = join(" & ", "\\hspace*{1.2em}{\\scriptsize\\($rhs_label\\)}", "", @row_s) . $row_end . "\n";
        $_ = $pass_line . $a_line . $s_line . "\\addlinespace[0.15em]\n";
      }
    }
  ' "$file"
  perl -0777 -i -pe 's/\\addlinespace\[0\.15em\]\n\\bottomrule/\n\\bottomrule/g' "$file"
  format_appendix_papi_subrow_layout "$file" "$pass_width"
}

reduce_papi_columns_for_main() {
  local src_file="$1"
  local dst_file="$2"

  mkdir -p "$(dirname "$dst_file")"
  cp "$src_file" "$dst_file"
  perl -i -pe '
    if (/ & / && /\\\\$/) {
      s/^((?:[^&\n]*&){2})\s*[^&\n]*\s*&\s*/$1 /;
    }
    s/\bAm\/Sm\b/Um\/Fm/g;
    s/\bAi\/Si\b/Ui\/Fi/g;
    s/\bA\/S\b/Um\/Fm/g;
    s/ CYC=CPU cycles; ?//g;
    s/pass\.([A-Z])/pass. $1/g;
  ' "$dst_file"
  perl -0777 -i -pe '
    s/^\\setlength\{\\tabcolsep\}\{[^}]+\}$/\\setlength{\\tabcolsep}{1.0pt}/mg;
  ' "$dst_file"

  local pass_width
  local value_width
  pass_width="$(
    awk '
      BEGIN { in_body = 0; max_len = 0 }
      /^\\midrule$/ { in_body = 1; next }
      /^\\bottomrule$/ { in_body = 0 }
      in_body && / & / {
        name = $0
        sub(/[[:space:]]*&.*/, "", name)
        gsub(/\\_/, "_", name)
        gsub(/[{}]/, "", name)
        len = length(name)
        if (len > max_len) max_len = len
      }
      END {
        width = 0.11 * max_len + 0.28
        if (width < 2.15) width = 2.15
        if (width > 2.50) width = 2.50
        printf "%.2fcm", width
      }
    ' "$dst_file"
  )"
  value_width="$(
    awk '
      BEGIN { in_body = 0; max_len = 0 }
      /^\\midrule$/ { in_body = 1; next }
      /^\\bottomrule$/ { in_body = 0 }
      in_body && / & / {
        line = $0
        sub(/\\\\.*/, "", line)
        n = split(line, parts, /[[:space:]]*&[[:space:]]*/)
        for (i = 3; i <= n; i++) {
          cell = parts[i]
          gsub(/\\textbf\{/, "", cell)
          gsub(/[{}]/, "", cell)
          gsub(/[[:space:]]+/, "", cell)
          len = length(cell)
          if (len > max_len) max_len = len
        }
      }
      END {
        # The main-paper Compiler PAPI table needs metric cells wider than the
        # appendix version, and dropping CYC frees space for wider remaining
        # metric columns in the main paper.
        width = 0.11 * max_len + 0.22
        if (width < 1.75) width = 1.75
        if (width > 1.75) width = 1.75
        printf "%.2fcm", width
      }
    ' "$dst_file"
  )"

  PAPI_PASS_WIDTH="$pass_width" PAPI_VALUE_WIDTH="$value_width" PAPI_METRIC_COLS="6" perl -0777 -i -pe '
    my $w = $ENV{PAPI_PASS_WIDTH};
    my $vw = $ENV{PAPI_VALUE_WIDTH};
    my $cols = $ENV{PAPI_METRIC_COLS};
    my $at = chr(64);
    my $spec = "\\begin{tabular}{" . $at . "{}>{\\raggedright\\arraybackslash}p{$w} c *{$cols}{>{\\raggedleft\\arraybackslash}p{$vw}}" . $at . "{}}";
    s#^\\begin\{tabular\}\{.*\}$#$spec#m;
  ' "$dst_file"
  perl -0777 -i -pe '
    s#\\caption\{Per-pass PAPI counters for#\\caption{Per-pass PAPI\\protect\\footnotemark{} counters for#;
    s#Each cell reports median counter-value pairs per pass\.#Cells show median \\(\\mathcal{C}_{gm}/\\mathcal{C}_{f}\\) pairs per pass; \\(\\mathcal{C}_{gm}\\)=Gibbon-flat mutable, \\(\\mathcal{C}_{f}\\)=factored mutable.#;
    s#\\end\{table\*\}\s*\z#\\end{table*}\n\\footnotetext{PAPI project site: \\url{https://icl.utk.edu/papi/}}#s;
  ' "$dst_file"
}

wrap_pass_names() {
  local file="$1"
  perl -0777 -i -pe '
    s{^([A-Za-z][A-Za-z0-9]*Pass)(?=\s*&)}{
      my $name = $1;
      my $wrapped = $name;
      $wrapped =~ s/([a-z0-9])([A-Z])/$1\\\\$2/g;
      "\\makecell[l]{" . $wrapped . "}"
    }mge;
  ' "$file"
}

round_table_numbers_two_decimals() {
  local file="$1"
  perl -0777 -i -pe '
    s/([0-9]+\.[0-9]+)e([+-]?[0-9]+)/sprintf("%.2f", $1) . "e" . $2/ge;
    s/([0-9]+\.[0-9]+)(?!e[+-]?[0-9])/sprintf("%.2f", $1)/ge;
    s/([0-9]+\.[0-9]{2})0+(?=e[+-]?[0-9]+)/$1/g;
  ' "$file"
}

convert_runtime_table_seconds_to_ms() {
  local file="$1"
  perl -i -pe '
    BEGIN { $in_body = 0; $done_runtime = 0; }
    if (!$done_runtime && /^\\midrule$/) {
      $in_body = 1;
    } elsif ($in_body && /^\\bottomrule$/) {
      $in_body = 0;
      $done_runtime = 1;
    } elsif ($in_body && / & /) {
      my $line = $_;
      my $row_end = "";
      $line =~ s/\s*\\\\\s*$// and $row_end = " \\\\";
      my @parts = split(/\s*&\s*/, $line, -1);
      if (@parts >= 10) {
        for my $idx (4, 5, 6) {
          my $cell = $parts[$idx];
          $cell =~ s/^\s+//;
          $cell =~ s/\s+$//;
          next if $cell eq "" || $cell eq "--";
          my $bold = ($cell =~ s/^\\textbf\{(.*)\}$/$1/);
          $cell =~ s/\$\\pm\$.*$//;
          $bold = 1 if (!$bold && $cell =~ s/^\\textbf\{(.*)\}$/$1/);
          next unless $cell =~ /^([0-9]+(?:\.[0-9]+)?(?:e[+-]?[0-9]+)?)$/i;
          my $ms = ($1 + 0) * 1000.0;
          my $txt = sprintf("%.3f", $ms);
          $parts[$idx] = $bold ? "\\textbf{$txt}" : $txt;
        }
        $_ = join(" & ", @parts) . $row_end . "\n";
      }
    }
  ' "$file"
}

convert_summary_seconds_to_ms() {
  local file="$1"
  perl -i -pe '
    BEGIN { $in_body = 0; }
    if (/^\\midrule$/) {
      $in_body = 1;
    } elsif (/^\\bottomrule$/) {
      $in_body = 0;
    } elsif ($in_body && / & /) {
      my $line = $_;
      my $row_end = "";
      $line =~ s/\s*\\\\\s*$// and $row_end = " \\\\";
      my @parts = split(/\s*&\s*/, $line, -1);
      if (@parts >= 18) {
        for my $idx (3, 4, 5, 8, 9, 10, 13, 14, 15) {
          my $cell = $parts[$idx];
          $cell =~ s/^\s+//;
          $cell =~ s/\s+$//;
          next if $cell eq "" || $cell eq "--";
          my $bold = ($cell =~ s/^\\textbf\{(.*)\}$/$1/);
          $cell =~ s/\$\\pm\$.*$//;
          $bold = 1 if (!$bold && $cell =~ s/^\\textbf\{(.*)\}$/$1/);
          next unless $cell =~ /^([0-9]+(?:\.[0-9]+)?(?:e[+-]?[0-9]+)?)$/i;
          my $ms = ($1 + 0) * 1000.0;
          my $txt = sprintf("%.3f", $ms);
          $parts[$idx] = $bold ? "\\textbf{$txt}" : $txt;
        }
        $_ = join(" & ", @parts) . $row_end . "\n";
      }
    }
  ' "$file"
}

bold_speedups_gt_one() {
  local file="$1"
  perl -0777 -i -pe '
    my $d = chr(36);
    my $tok = $d . q{\times} . $d;
    # Reset simple pre-existing speedup bolding so we can apply one consistent rule.
    s/\\textbf\{([0-9]+(?:\.[0-9]+)?)\s*\$\\times\$\}/$1 . $tok/ge;
    # Bold all numeric speedups strictly greater than 1.0x.
    s/([0-9]+(?:\.[0-9]+)?)\s*\$\\times\$/
      my $v = $1 + 0;
      $v > 1.0 ? "\\textbf{" . $1 . $tok . "}" : $1 . $tok
    /gex;
  ' "$file"
}

strip_error_bars() {
  local file="$1"
  perl -0777 -i -pe '
    # Remove per-cell standard-error terms like: 0.013$\pm$0.000 -> 0.013
    s/\$\\pm\$\s*[-+]?[0-9]+(?:\.[0-9]+)?(?:e[+-]?[0-9]+)?//g;
    # If any runtime caption still mentions ± text, remove that phrase.
    s/, with \$\\pm\$ as standard error across --iterate runs//g;
  ' "$file"
}

strip_speedup_times() {
  local file="$1"
  perl -0777 -i -pe '
    # Remove trailing "x" marker in speedup cells, e.g. 1.34$\times$ -> 1.34
    s/\$\\times\$//g;
  ' "$file"
}

normalize_summary_display_names() {
  local file="$1"
  perl -0777 -i -pe '
    s/^DecisionTree(?=\s*&)/DTree/mg;
    s/^ObjectGraph(?=\s*&)/ObjGraph/mg;
    s/^PiecewiseFunctions(?=\s*&)/PWF/mg;
    s/^ColorOctree(?=\s*&)/ColOct/mg;
    s/^LinearListReduction(?=\s*&)/LLR/mg;
    s/^ReduceNestedList(?=\s*&)/RNL/mg;
    s/^reduceNestedList(?=\s*&)/RNL/mg;
    s/^TernaryTree(?=\s*&)/TTree/mg;
  ' "$file"
}

normalize_pass_display_names() {
  local file="$1"
  perl -0777 -i -pe '
    s/^countInRange tight\\_box(?=\s*&)/countInRange/mg;
    s/^twoPointCorrelation bin\\_8\\_16(?=\s*&)/twoPointCorrelation/mg;
  ' "$file"
}

group_summary_microbenchmarks() {
  local file="$1"
  perl -0777 -i -pe '
    my @micro_order = qw(LinearListReduction reduceNestedList List MonoTree TernaryTree);
    my %is_micro = map { $_ => 1 } @micro_order;
    my $sep = chr(92) . "cmidrule(lr){1-12}";
    if ($_ =~ /^.*?& fields & bufs.*\\\\/m) {
      my $hdr = $&;
      my $amps = () = ($hdr =~ /&/g);
      $sep = chr(92) . "cmidrule(lr){1-" . ($amps + 1) . "}";
    }
    s#(\\midrule\n)(.*?)(\n\\bottomrule)#
      my ($start, $body, $end) = ($1, $2, $3);
      my @lines = grep { /\S/ } split(/\n/, $body);
      my %micro_lines;
      my @other_lines;
      for my $line (@lines) {
        if ($line =~ /^([^&]+?)\s*&/) {
          my $name = $1;
          $name =~ s/\s+$//;
          if ($is_micro{$name}) {
            $micro_lines{$name} = $line;
            next;
          }
        }
        push @other_lines, $line;
      }
      my @ordered_micro = grep { exists $micro_lines{$_} } @micro_order;
      if (!@ordered_micro) {
        $start . $body . $end
      } else {
        $start
          . join("\n", @other_lines)
          . "\n" . $sep . "\n"
          . join("\n", map { $micro_lines{$_} } @ordered_micro)
          . $end
      }
    #se;
  ' "$file"
}

append_summary_geomean_row() {
  local file="$1"
  perl -0777 -i -pe '
    if (/\A(.*?)(\\midrule\n)(.*?)(\n\\bottomrule)(.*)\z/s) {
      my ($prefix, $start, $body, $end, $suffix) = ($1, $2, $3, $4, $5);
      my @speed_cols = (6, 7, 11, 12, 16, 17);
      my @sumlog = (0) x scalar(@speed_cols);
      my @count  = (0) x scalar(@speed_cols);

      my @lines = grep { /\S/ } split(/\n/, $body);
      for my $line (@lines) {
        next if $line =~ /^\\(?:cmidrule|midrule)\b/;
        next if $line =~ /^\\textbf\{Geomean\}\b/;
        next unless $line =~ / & /;

        my $trimmed = $line;
        $trimmed =~ s/\s*\\\\\s*$//;
        my @parts = split(/\s*&\s*/, $trimmed, -1);
        next unless @parts >= 18;

        for my $i (0 .. scalar(@speed_cols) - 1) {
          my $cell = $parts[$speed_cols[$i]];
          $cell =~ s/\\textbf\{([^}]*)\}/$1/g;
          $cell =~ s/[{}]//g;
          $cell =~ s/^\s+//;
          $cell =~ s/\s+$//;
          next if $cell eq "" || $cell eq "--";
          next unless $cell =~ /([0-9]+(?:\.[0-9]+)?(?:e[+-]?[0-9]+)?)/i;
          my $v = $1 + 0;
          next if $v <= 0;
          $sumlog[$i] += log($v);
          $count[$i] += 1;
        }
      }

      my @gm;
      for my $i (0 .. scalar(@speed_cols) - 1) {
        if ($count[$i] == 0) {
          push @gm, "--";
          next;
        }
        my $v = exp($sumlog[$i] / $count[$i]);
        my $txt = sprintf("%.6f", $v);
        $txt =~ s/0+$//;
        $txt =~ s/\.$//;
        push @gm, ($v > 1.0) ? "\\textbf{$txt}" : $txt;
      }

      my @row = (
        "\\textbf{Geomean}", "", "",
        "", "", "", $gm[0], $gm[1],
        "", "", "", $gm[2], $gm[3],
        "", "", "", $gm[4], $gm[5],
      );
      my $geo = join(" & ", @row) . " \\\\";
      my $body_out = $body;
      $body_out =~ s/\s+\z//;
      $_ = $prefix . $start . $body_out . "\n\\midrule\n" . $geo . $end . $suffix;
    }
  ' "$file"
}

split_program_tables() {
  local full_file="$1"
  local runtime_file="$2"
  local papi_file="$3"

  mkdir -p "$(dirname "$runtime_file")" "$(dirname "$papi_file")"

  awk '
    BEGIN {count = 0}
    /^% -- Table: / {
      count++
      if (count == 2) exit
    }
    {print}
  ' "$full_file" > "$runtime_file"

  awk '
    BEGIN {count = 0; capture = 0}
    /^% -- Table: / {
      count++
      if (count == 2) capture = 1
    }
    capture {print}
  ' "$full_file" > "$papi_file"
}

programs=(
  Compiler
  DBQuery
  DecisionTree
  DomTree
  KDTree
  LinearListReduction
  reduceNestedList
  List
  MonoTree
  ObjectGraph
  OctTree
  PiecewiseFunctions
  TernaryTree
  Trie
)

papi_caption_verbose=1
verbose_perf_caption_prog="KDTree"

mkdir -p "$out_dir"

extract_section() {
  local src_file="$1"
  local header="$2"
  awk -v hdr="$header" '
    BEGIN {capture = 0; found = 0}
    {
      line = $0
      sub(/\r$/, "", line)
      sub(/[[:space:]]+$/, "", line)
      sub(/^[[:space:]]+/, "", line)
    }
    line == hdr {capture = 1; found = 1}
    capture && line ~ /^% -- Table: / && line != hdr {exit}
    capture {print line}
    END { if (!found) exit 2 }
  ' "$src_file"
}

extract_tabular_block() {
  local table_file="$1"
  awk '
    /\\begin\{tabular\}/ {capture = 1}
    capture {print}
    /\\end\{tabular\}/ {exit}
  ' "$table_file"
}

extract_first_table_after_header() {
  local src_file="$1"
  local header="$2"
  awk -v hdr="$header" '
    BEGIN {capture = 0; found = 0; table_started = 0}
    {
      line = $0
      sub(/\r$/, "", line)
      sub(/[[:space:]]+$/, "", line)
      sub(/^[[:space:]]+/, "", line)
    }
    line == hdr {capture = 1; found = 1}
    capture {print line}
    capture && line ~ /^\\begin\{table\*?\}/ {table_started = 1}
    capture && table_started && line ~ /^\\end\{table\*?\}/ {exit}
    END { if (!found) exit 2 }
  ' "$src_file"
}

for prog in "${programs[@]}"; do
  out_file="$out_dir/$prog.tex"
  tmp_file="$(mktemp)"

  # Main per-program runtime table (required).
  if ! extract_section "$src" "% -- Table: $prog --" > "$tmp_file"; then
    echo "Error: failed to extract runtime table for program '$prog' from $src" >&2
    rm -f "$tmp_file"
    exit 1
  fi

  # Per-program PAPI tables (optional in old formats, present in latest figures format).
  papi_mut_file="$(mktemp)"
  papi_imm_file="$(mktemp)"
  has_papi_mut=0
  has_papi_imm=0
  papi_mut_variant=""
  papi_imm_variant=""

  # Newer generator uses Am/Sm; older one uses A/S.
  if extract_section "$src" "% -- Table: $prog PAPI Am/Sm --" > "$papi_mut_file" 2>/dev/null; then
    has_papi_mut=1
    papi_mut_variant="Am/Sm"
  elif extract_section "$src" "% -- Table: $prog PAPI A/S --" > "$papi_mut_file" 2>/dev/null; then
    has_papi_mut=1
    papi_mut_variant="A/S"
  fi
  # Optional immutable variant (present in some formats).
  if extract_section "$src" "% -- Table: $prog PAPI Ai/Si --" > "$papi_imm_file" 2>/dev/null; then
    has_papi_imm=1
    papi_imm_variant="Ai/Si"
  fi

  if [[ $has_papi_mut -eq 1 && $has_papi_imm -eq 1 ]]; then
    {
      echo "% -- Table: $prog PAPI (combined) --"
      echo '\begin{table*}[t]'
      echo '\centering'
      echo '\captionsetup{justification=raggedright,singlelinecheck=false}'
      if [[ $papi_caption_verbose -eq 1 ]]; then
        caption_text="Per-pass PAPI counters for \\texttt{$prog}. Each cell reports median counter-value pairs per pass. CYC=CPU cycles; L1D/L1I/L2D/L2I=cache misses; LLC=LLC load misses."
        papi_caption_verbose=0
      else
        caption_text="Per-pass PAPI counters for \\texttt{$prog}."
      fi
      echo "\\caption{$caption_text}"
      echo "\\label{tab:${prog}_papi}"
      echo '\footnotesize'
      echo '\setlength{\tabcolsep}{1pt}'
      echo "\\textbf{$papi_mut_variant}\\par"
      extract_tabular_block "$papi_mut_file"
      echo '\par\medskip'
      echo "\\textbf{$papi_imm_variant}\\par"
      extract_tabular_block "$papi_imm_file"
      echo '\end{table*}'
    } >> "$tmp_file"
  else
    if [[ $has_papi_mut -eq 1 ]]; then
      cat "$papi_mut_file" >> "$tmp_file"
    fi
    if [[ $has_papi_imm -eq 1 ]]; then
      cat "$papi_imm_file" >> "$tmp_file"
    fi
  fi

  rm -f "$papi_mut_file" "$papi_imm_file"

  mv "$tmp_file" "$out_file"

  if [[ ! -s "$out_file" ]]; then
    echo "Error: failed to extract table content for program '$prog' from $src" >&2
    exit 1
  fi

  # Normalize an extra trailing brace emitted by the generator in caption lines.
  sed -i -E 's/\.}}$/\.}/' "$out_file"
  # Remove any anchor leftovers from prior sync formats.
  sed -i -E '/^[[:space:]]*\\?phantomsection[[:space:]]*$/d' "$out_file"
  perl -i -pe '
    s#^% -- Table: (.+) PAPI (?:A/S|Am/Sm|Um/Fm|Ai/Si|Ui/Fi) --$#% -- Table: $1 PAPI --#;
  ' "$out_file"

  # Formatting for paper layout:
  # - Use flexible float placement to avoid large end-of-page whitespace.
  # - Keep tables unscaled; width is controlled with column specs and tabcolsep.
  sed -i -E 's/^\\begin\{table\}\[[^]]*\]/\\begin{table}[!htbp]/' "$out_file"
  perl -0777 -i -pe '
    s/\\centering\n(?!\\captionsetup\{justification=raggedright,singlelinecheck=false\}\n)/\\centering\n\\captionsetup{justification=raggedright,singlelinecheck=false}\n/g;
  ' "$out_file"

  # Runtime/PAPI layout normalization:
  # - Compute runtime tabular columns from the runtime header and force a matching tabular spec.
  #   (Fixes off-by-one alignment shifts when the source emits an extra column in the spec.)
  # - Drop "Total" rows in both runtime and PAPI tables.
  # - Collapse redundant \midrule left behind after Total-row removal.
  perl -0777 -i -pe '
    if (/\\textbf\{Pass\} & \\textbf\{T\} & \\textbf\{Uses\}([^\n]*)\\\\/) {
      my $hdr_tail = $1;
      my $amp = () = ($hdr_tail =~ /&/g);
      my $cols = 3 + $amp;
      my @spec = ("l", "c", "c");
      push @spec, ("r") x ($cols - 3) if $cols > 3;
      my $spec = join(" ", @spec);
      s#(\\begin\{tabular\}\{)[^}]*(\}\n\\toprule\n\\textbf\{Pass\} & \\textbf\{T\} & \\textbf\{Uses\})#$1$spec$2#s;
    }
    s#(% -- Table: [^\n]+ --)\n\1#$1#g;
    s#^\\textbf\{Geomean\}\s*&\s*&\s*&\s*&\s*&\s*&\s*#\\textbf{Geomean} & & & & & & & #mg;
    s#\n\\textbf\{Total\}[^\n]*\n#\n#g;
    s#\n\\midrule\s*\n\\midrule#\n\\midrule#g;
    s#\n\\midrule\n\\bottomrule#\n\\bottomrule#g;
  ' "$out_file"
  rename_runtime_symbols "$out_file"

  # PAPI readability:
  # - Remove repeated variant suffixes from each header cell (caption already states it).
  # - Slightly increase row height for denser numeric tables.
  perl -0777 -i -pe '
    s#(\\caption\{Per-pass PAPI counters[^\n]*\n\\label\{[^\n]*\}\n\\(?:small|footnotesize)\n)(?!\\renewcommand\{\\arraystretch\})#$1\\renewcommand{\\arraystretch}{1.10}\n#g;
    s#\\textbf\{Pass\} & \\textbf\{T\} & \\textbf\{CYC \([^}]+\)\} & \\textbf\{INS \([^}]+\)\} & \\textbf\{L1D \([^}]+\)\} & \\textbf\{L1I \([^}]+\)\} & \\textbf\{L2D \([^}]+\)\} & \\textbf\{L2I \([^}]+\)\} & \\textbf\{LLC \([^}]+\)\} \\\\#\\textbf{Pass} & \\textbf{T} & \\textbf{CYC} & \\textbf{INS} & \\textbf{L1D} & \\textbf{L1I} & \\textbf{L2D} & \\textbf{L2I} & \\textbf{LLC} \\\\#g;
  ' "$out_file"

  # Local caption overrides (keep data sync from Gibbon, but shorten prose).
  perl -i -pe '
    s#\\caption\{Per-pass performance for \\texttt\{([^}]*)\}, ADT has ([0-9]+) fields, SoA uses ([0-9]+) buffers \(mutable \+ immutable cursors\)\. Times are median per iteration \(s\); \$\\pm\$ shows standard error of the mean across --iterate runs\. T: F=fold, M=map\. Uses: fields accessed / total \(recursive \+ non-recursive\)\. Dead\\%: fraction of fields not accessed by this pass\. Speedup \$\{>\}1\{\\times\}\$ means SoA is faster\. OOM = out of memory\.\}#\\caption{Per-pass performance for \\texttt{$1}. ADT fields: $2; factored buffers: $3. Times are median per-iteration runtime (ms), with \$\\pm\$ as standard error across --iterate runs. Here, \\(\\mathcal{S}_{gm}=\\mathcal{R}_{gi}/\\mathcal{R}_{gm}\\). T: F=fold, M=map; Uses: accessed/total fields; Dead\\%: unused-field fraction. OOM = out of memory.}#g;
    s#\\caption\{Per-pass PAPI counters for \\texttt\{([^}]*)\} \((A/S|Am/Sm|Ai/Si)\)\. Each cell is median counter value pair per pass\. Counter headers are abbreviated for compactness: CYC=CPU cycles, L1D/L1I/L2D/L2I=cache misses, LLC=LLC load misses\.\}#\\caption{Per-pass PAPI counters for \\texttt{$1}. Each cell reports median counter-value pairs per pass. CYC=CPU cycles; L1D/L1I/L2D/L2I=cache misses; LLC=LLC load misses.}#g;
  ' "$out_file"

  # Keep verbose performance-caption notes for the first runtime table discussed
  # in the main text, and shorten repetitive captions for the rest.
  if [[ "$prog" != "$verbose_perf_caption_prog" ]]; then
    perl -i -pe '
      s#\\caption\{Per-pass performance for \\texttt\{([^}]*)\}\. ADT fields: ([0-9]+); factored buffers: ([0-9]+)\.[^\n]*\\mathcal\{S\}_\{gm\}=\\mathcal\{R\}_\{gi\}/\\mathcal\{R\}_\{gm\}[^\n]*\}#\\caption{Per-pass performance for \\texttt{$1}. ADT fields: $2; factored buffers: $3. Times in ms.}#g;
      s#\\caption\{Per-pass PAPI counters for \\texttt\{([^}]*)\} \((?:A/S|Am/Sm) and Ai/Si\)\. Each cell reports median counter-value pairs per pass\. CYC=CPU cycles; L1D/L1I/L2D/L2I=cache misses; LLC=LLC load misses\.\}#\\caption{Per-pass PAPI counters for \\texttt{$1}.}#g;
      s#\\caption\{Per-pass PAPI counters for \\texttt\{([^}]*)\} \((A/S|Am/Sm|Ai/Si)\)\. Each cell reports median counter-value pairs per pass\. CYC=CPU cycles; L1D/L1I/L2D/L2I=cache misses; LLC=LLC load misses\.\}#\\caption{Per-pass PAPI counters for \\texttt{$1}.}#g;
    ' "$out_file"
  fi

  perl -i -pe '
    s/\\texttt\{reduceNestedList\}/\\texttt{ReduceNestedList}/g;
  ' "$out_file"
  perl -i -pe '
    s/\bSoA uses ([0-9]+) buffers\b/fully factored layout uses $1 buffers/g;
    s/\bSoA buffers\b/factored buffers/g;
    s/means SoA is faster/favors the factored layout/g;
    s/favors SoA/favors the factored layout/g;
    s/AoS mutable/Gibbon-flat mutable/g;
    s/AoS immutable/Gibbon-flat immutable/g;
    s/SoA mutable/factored mutable/g;
    s/SoA immutable/factored immutable/g;
    s/fully factored buffers/factored buffers/g;
    s/fully factored layout uses/factored layout uses/g;
    s/favors the fully factored layout/favors the factored layout/g;
    s/unfactored mutable/flattened mutable/g;
    s/unfactored immutable/flattened immutable/g;
    s/fully factored mutable/factored mutable/g;
    s/fully factored immutable/factored immutable/g;
  ' "$out_file"

  # Numeric postprocessing:
  # 1) non-scientific decimals -> 3 decimal places
  # 2) scientific notation exponent format: e+04 -> e4, e-06 -> e-6
  convert_runtime_table_seconds_to_ms "$out_file"
  perl -i -pe '
    s/([0-9]+(?:\.[0-9]+)?)e([+-])0*([0-9]+)/$1 . "e" . ($2 eq "+" ? "" : "-") . $3/ge;
    s/([0-9]+\.[0-9]+)(?!e[+-]?[0-9])/sprintf("%.3f", $1)/ge;
  ' "$out_file"

  strip_error_bars "$out_file"
  bold_speedups_gt_one "$out_file"
  strip_speedup_times "$out_file"
  normalize_eval_table_output "$out_file"

  if ! has_table_begin "$out_file"; then
    echo "Error: extracted file is not a table for program '$prog': $out_file" >&2
    exit 1
  fi

  if ! has_tabular_block "$out_file"; then
    echo "Error: extracted file is missing a tabular block for '$prog': $out_file" >&2
    exit 1
  fi

  split_program_tables "$out_file" "$runtime_out_dir/$prog.tex" "$papi_out_dir/$prog.tex"

  if [[ ! -s "$papi_out_dir/$prog.tex" && -f "$papi_seed_dir/$prog.tex" ]]; then
    cp "$papi_seed_dir/$prog.tex" "$papi_out_dir/$prog.tex"
  fi

  normalize_eval_table_output "$runtime_out_dir/$prog.tex"
  normalize_eval_table_output "$papi_out_dir/$prog.tex"
  normalize_pass_display_names "$runtime_out_dir/$prog.tex"
  normalize_pass_display_names "$papi_out_dir/$prog.tex"
  rename_runtime_symbols "$runtime_out_dir/$prog.tex"
  set_tabcolsep "$runtime_out_dir/$prog.tex" "2pt"
  set_tabcolsep "$papi_out_dir/$prog.tex" "1pt"
  set_table_environment "$runtime_out_dir/$prog.tex" "table" "t"
  set_table_environment "$papi_out_dir/$prog.tex" "table*" "t"
  format_papi_layout "$papi_out_dir/$prog.tex"
  shrink_column_headers "$runtime_out_dir/$prog.tex"
  shrink_column_headers "$papi_out_dir/$prog.tex"

  case "$prog" in
    Compiler|KDTree|DecisionTree)
      round_table_numbers_two_decimals "$runtime_out_dir/$prog.tex"
      ;;
  esac

  if [[ "$prog" == "Compiler" ]]; then
    reduce_papi_columns_for_main "$papi_out_dir/$prog.tex" "$papi_main_out_dir/$prog.tex"
    normalize_pass_display_names "$papi_main_out_dir/$prog.tex"
    shrink_column_headers "$papi_main_out_dir/$prog.tex"
    perl -i -pe 's/\\label\{tab:Compiler_papi_mut\}/\\label{tab:Compiler_papi_mut_full}/g' "$papi_out_dir/$prog.tex"
    round_table_numbers_two_decimals "$papi_out_dir/$prog.tex"
    round_table_numbers_two_decimals "$papi_main_out_dir/$prog.tex"
  fi

  appendix_papi_variant="${papi_mut_variant:-$papi_imm_variant}"
  expand_appendix_papi_pairs_to_subrows "$papi_out_dir/$prog.tex" "$appendix_papi_variant"
done

if [[ -n "$summary_out_file" ]]; then
  summary_tmp_file="$(mktemp)"
  summary_header="% -- Table 1: Summary by pass type --"

  if ! extract_first_table_after_header "$src" "$summary_header" > "$summary_tmp_file"; then
    echo "Error: failed to extract summary table from $src" >&2
    rm -f "$summary_tmp_file"
    exit 1
  fi

  mkdir -p "$(dirname "$summary_out_file")"
  mv "$summary_tmp_file" "$summary_out_file"

  if [[ ! -s "$summary_out_file" ]]; then
    echo "Error: failed to extract summary table content from $src" >&2
    exit 1
  fi

  sed -i -E 's/^\\begin\{table\}\[[^]]*\]/\\begin{table}[!htbp]/' "$summary_out_file"
  perl -0777 -i -pe '
    s/\\centering\n(?!\\captionsetup\{justification=raggedright,singlelinecheck=false\}\n)/\\centering\n\\captionsetup{justification=raggedright,singlelinecheck=false}\n/g;
  ' "$summary_out_file"
  perl -0777 -i -pe '
    s#\\caption\{.*?\}\n\\label\{tab:summary\}#\\caption{Table shows end-to-end time per application; Fold/Map are end-to-end totals for all folds/all maps. Fd and B denote ADT fields and factored buffers. Here, \\(\\mathcal{R}_{gm}\\), \\(\\mathcal{R}_{gi}\\), and \\(\\mathcal{R}_{f}\\) denote optimized Gibbon-flat, baseline Gibbon-flat, and factored runtimes (s); \\(\\mathcal{S}_{fo}=\\mathcal{R}_{gm}/\\mathcal{R}_{f}\\) and \\(\\mathcal{S}_{fb}=\\mathcal{R}_{gi}/\\mathcal{R}_{f}\\). Abbreviations: DTree=DecisionTree, ObjGraph=ObjectGraph, ColOct=ColorOctree, TTree=TernaryTree, LLR=LinearListReduction, RNL=ReduceNestedList, PWF=PiecewiseFunctions.}\n\\label{tab:summary}#s;
  ' "$summary_out_file"
  perl -i -pe '
    s/\\textbf\{bold\} marks \$\{>\}1\.1\{\\times\}\$/\\textbf{bold} marks \$\{>\}1\{\\times\}\$/g;
  ' "$summary_out_file"

  strip_error_bars "$summary_out_file"
  bold_speedups_gt_one "$summary_out_file"
  strip_speedup_times "$summary_out_file"
  group_summary_microbenchmarks "$summary_out_file"
  append_summary_geomean_row "$summary_out_file"
  normalize_summary_display_names "$summary_out_file"
  normalize_eval_table_output "$summary_out_file"
  set_tabcolsep "$summary_out_file" "1pt"
  set_table_environment "$summary_out_file" "table*" "t"
  format_summary_layout "$summary_out_file"
  rename_runtime_symbols "$summary_out_file"
  shrink_column_headers "$summary_out_file"
  round_table_numbers_two_decimals "$summary_out_file"
  format_summary_layout "$summary_out_file"

  if ! has_table_begin "$summary_out_file"; then
    echo "Error: extracted summary file is not a table: $summary_out_file" >&2
    exit 1
  fi

  if ! has_tabular_block "$summary_out_file"; then
    echo "Error: extracted summary file is missing a tabular block: $summary_out_file" >&2
    exit 1
  fi
fi

echo "Synced ${#programs[@]} per-program tables to $out_dir"
if [[ -n "$summary_out_file" ]]; then
  echo "Synced summary table to $summary_out_file"
fi
