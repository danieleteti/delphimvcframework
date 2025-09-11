{{ for person of people }}"{{ person.guid }}","{{ person.FirstName }}","{{ person.LastName }}",{{ person.Age }}
{{ onbegin }}"guid","first name","last name","age"
{{ end }}
