function! s:sort_range(pat, start, end)
  let headlines = s:otl2list(getline(a:start, a:end))
  call sort(headlines)
  call setline(s:list2otl(headlines))
endfunction

function! s:otl2list(list)
  let the_list = copy(a:list)
  let min_indent = min(map(the_list, 'len(substitute(v:val,''\(^\s*\).*$'',''\1'', ""))'))
  let result = []
  for line in the_list
    
