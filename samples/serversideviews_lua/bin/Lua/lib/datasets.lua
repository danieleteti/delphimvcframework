function open_dataset(ud)
  _dataset_open(ud);
  m = {
    pointer = ud,
    eof = function()
      return _dataset_eof(ud)
    end,
    movenext = function()
      _dataset_move_next(ud)
    end,
    get = function(fieldname)
      return _dataset_field_by_name(ud, fieldname)
    end,
    close = function()
      _dataset_close(ud)
    end, 
    first = function()
      _dataset_first(ud)
    end,   
    is_open = function()
	  return _dataset_is_open(ud) 
    end,          
    open = function()
	  _dataset_open(ud)
    end          	
  };
  
  return m;
end