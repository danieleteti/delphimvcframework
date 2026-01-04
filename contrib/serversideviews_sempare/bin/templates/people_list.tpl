{{ extends('page'); block 'body' }}
<div class="row_fluid">
    <div class="col-sm-12">
        {{ for person of people }}
                <tr>
                    <td>{{ (_loop_idx_ + 1) }}</td>
                    <td>{{ person.FirstName }}</td>
                    <td>{{ person.LastName }}</td>
                    <td>{{ person.Age }}</td>
                    <td class="text-right">
                        {{ include('view_person_link', person) }}
                    </td>
                </tr>
        {{ onempty }}
                &lt;&lt;No People Found&gt;&gt;
        {{ onbegin }}
                <table class="table table-striped">
                    <thead>
                        <tr>
                            <th>#</th>
                            <th>First name</th>
                            <th>Last name</th>
                            <th>Age</th>
                            <th>&nbsp;</th>
                        </tr>
                    </thead>
        {{ onend }}
                    <tbody>
                    </tbody>
                </table>
        {{ end }}
    </div>
</div>
<br>
<div class="row_fluid">
    <div class="col-sm-2">
        <a class="btn btn-default btn-block" href="/people/formats/csv">Export as CSV</a>
    </div>
    <div class="col-sm-2 col-sm-offset-8">
        <a class="btn btn-primary btn-block" href="/new">Add New Person</a>
    </div>
</div>
{{ end; end }}
