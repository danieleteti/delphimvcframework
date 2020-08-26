import React, { Component } from 'react';
import api from '../../services/api';
import './index.css';

class Customer extends Component {
    constructor(props) {
        super(props);
        this.state = { customer: { id: 0 } };

        this.handleInputChange = this.handleInputChange.bind(this);
        this.handleSubmit = this.handleSubmit.bind(this);
        this.handleCancel = this.handleCancel.bind(this);

    }

    async componentDidMount() {
        const { id } = this.props.match.params;
        const response = await api.get(`/customers/${id}`)
        this.setState({ customer: response.data });
    }

    handleInputChange = (event) => {
        const { name, value } = event.target;
        const {customer} = this.state;
        const newCustomer = {...customer, [name]: value };
        this.setState({'customer': newCustomer});
    }


    async handleSubmit(event) {
        const {customer} = this.state;

        if (customer.id == 0) {
            await api.post(`/customers`, customer);
        } else {
            const id = customer.id;
            await api.put(`/customers/${id}`, customer);
           
        }

        this.props.history.push("/");
        event.preventDefault();
    }

    handleCancel(event) {
        this.props.history.push("/");
    }


    render() {
        const { customer } = this.state;

        return (
            <>
                <nav className="navbar navbar-expand-lg navbar-light bg-light"><a className="navbar-brand" href="#">Customer Details</a></nav>
                <form>
                    <div className="container">

                        <div className="form-group">
                            <label htmlFor="inputID">ID</label>
                            <input type="text" readOnly className="form-control-plaintext" id="inputID" value={customer.id} />
                        </div>

                        <div className="form-group">
                            <label htmlFor="inputCode">Code</label>
                            <input type="text" name="Code" className="form-control" id="inputCode" placeholder="Enter Code" value={customer.Code} onChange={this.handleInputChange} />
                        </div>

                        <div className="form-group">
                            <label htmlFor="inputDescription">Description</label>
                            <input type="text" name="Description" className="form-control" id="inputDescription" placeholder="Enter Description" value={customer.Description} onChange={this.handleInputChange} />
                        </div>

                        <div className="form-group">
                            <label htmlFor="inputCity">City</label>
                            <input type="text" name="City" className="form-control" id="inputCity" placeholder="Enter City" value={customer.City} onChange={this.handleInputChange} />
                        </div>

                        <div className="form-group">
                            <label htmlFor="inputNote">Note</label>
                            <textarea className="form-control" name="Note" id="inputNote" rows="3" value={customer.Note} onChange={this.handleInputChange} />
                        </div>

                        <div className="form-group">
                            <label htmlFor="inputRating">Rating</label>
                            <select value={customer.rating} name="rating" className="custom-select mr-sm-2" id="inputRating" onChange={this.handleInputChange} >
                                <option value="0">0</option>
                                <option value="1">1</option>
                                <option value="2">2</option>
                                <option value="3">3</option>
                                <option value="4">4</option>
                                <option value="5">5</option>
                            </select>

                        </div>

                        <div className="form-group">
                            <button type="button" className="btn btn-primary" onClick={this.handleSubmit}>Save</button>
                            <button type="button" className="btn btn-danger" onClick={this.handleCancel}>Cancel</button>
                        </div>
                    </div>
                </form>
            </>
        )
    }
}

export default Customer;