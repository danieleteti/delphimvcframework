{{ extends('page'); block 'body' }}
<script>
  function doDelete(id) {
    if (confirm('Are you sure?')) {
      let form = document.getElementById("myForm");
      form.action = "/deleteperson";
      form.submit();
    }
  }
</script>
<div class="row_fluid">
  <div class="col-sm-12">
    <form class="form form-horizontal" id="myForm" name="myForm" method="POST" action="/people">
      <input type="hidden" value="{{person.guid}}" name="guid">
      <div class="row">
        <div class="col-sm-offset-2 col-sm-8">
          {{ if not person }}
          <h3>New Person</h3>
          {{ else }}
          <h3>Edit Person</h3>
          {{ end }}
        </div>
      </div>

      <div class="row">
        <div class="form-group">
          <label for="first_name" class="col-sm-2 control-label">First name</label>
          <div class="col-sm-4">
            <input type="text" value="{{ person.FirstName }}" class="form-control" id="first_name" placeholder="First name" name="first_name">
          </div>
        </div>
      </div>

      <div class="row">
        <div class="form-group">
          <label for="last_name" class="col-sm-2 control-label">Last name</label>
          <div class="col-sm-4">
            <input type="text" value="{{ person.LastName }}" class="form-control" id="last_name" placeholder="Last name" name="last_name">
          </div>
        </div>
      </div>

      <div class="row">
        <div class="form-group">
          <label for="age" class="col-sm-2 control-label">Age</label>
          <div class="col-sm-4">
            <input type="number" value="{{ person.Age }}" class="form-control" id="age" placeholder="Age" name="age">
          </div>
        </div>
      </div>


        <div class="row">
                <div class="form-group">
                        <label for="devices" class="col-sm-2 control-label">Devices</label>
                        <div class="col-sm-4">
                                <select name="devices" multiple class="form-control">
                                {{ for i in devices; with devices[i] }}
                                        <option value="{{ name }}" {{ if selected }}selected{{ end }}>{{ name}}</option>
                                {{ end; end }}
                                </select>
                                <span style="font-size: 80%">(Ctrl+Click to select multiple devices)</span>
                        </div>
                </div>
        </div>

      <div class="form-group">
        <div class="col-sm-offset-2 col-sm-2">
          <button type="button" class="btn btn-default btn-block" onclick="history.back()">Return to the list</button>
        </div>
        <div class="col-sm-2">
          {{ if person }}
          <button type="button" onclick="doDelete()" class="btn btn-primary btn-block">Delete</button> {{ else }}
          <button type="submit" class="btn btn-primary btn-block">Save</button> {{ end }}
        </div>
      </div>
    </form>
  </div>
</div>
{{ end; end }}
