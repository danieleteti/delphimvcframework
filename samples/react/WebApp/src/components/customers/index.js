import React, { Component } from 'react';
import api from '../../services/api';


class Customers extends Component {
    constructor(props) {
        super(props);

        this.handleAdd = this.handleAdd.bind(this);
        this.handleDelete = this.handleDelete.bind(this);
    }

    state = {
        customer: [],
    }

    async componentDidMount() {
        const response = await api.get('/customers');
        this.setState({ customer: response.data });

    }

    handleAdd() {
        this.props.history.push("/customer");
    }

    async handleDelete(id){
        await api.delete(`/customers/${id}`);

        const response = await api.get('/customers');
        this.setState({ customer: response.data });

    }

    render() {
        return (
            <>
                <nav className="navbar navbar-light bg-light justify-content-between">
                    <a className="navbar-brand" href="#">React DMVC Framework Demo</a>

                    <button className="btn btn-primary" onClick={this.handleAdd}>Add</button>

                </nav>
                <table className="table">
                    <thead>
                        <tr>
                            <th scope="col">#</th>
                            <th scope="col">Code</th>
                            <th scope="col">Description</th>
                            <th scope="col">City</th>
                            <th scope="col">Rating</th>
                            <th scope="col">Operations</th>
                        </tr>
                    </thead>
                    <tbody>
                        {this.state.customer.map(customer => (
                            <tr key={customer.id}>
                                <td>{customer.id}</td>
                                <td>{customer.Code}</td>
                                <td>{customer.Description}</td>
                                <td>{customer.City}</td>
                                <td>{customer.rating}</td>
                                <td>
                                    <a href={'/customer/' + customer.id} className="badge badge-light">edit</a>
                                    <a href="#" onClick={() => this.handleDelete(customer.id)} className="badge badge-danger">delete</a>
                                </td>
                            </tr>
                        ))}
                    </tbody>
                </table>
            </>
        )
    }
}

export default Customers;