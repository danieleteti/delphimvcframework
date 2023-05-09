import React, { Component } from 'react';
import api from '../../services/api';
import { useParams, useNavigate } from 'react-router-dom';
import './index.css';
import { useState, useEffect, useRef } from 'react';


const errors = {
    code: "",
    description: "",
    city: ""
}

function Error(props) {
    if (props.message === "")
        return <></>
    return <li className="error">❌ {props.message}</li>
}

function Errors(props) {
    if (!props.errors || (props.errors.length === 0))
        return null;
    return (
        <div className="errors">
            <h3>🐞 Errors</h3>
            <ul>
                {props.errors.map((msg) => <Error message={msg.message} />)}
            </ul>
        </div>
    )
}

function Customer(props) {
    const params = useParams();
    const [customer, setCustomer] = useState(null);
    const [errors, setErrors] = useState([]);
    const insertMode = useRef(false);
    const navigate = useNavigate();

    useEffect(() => {
        const { id } = params;
        insertMode.current = typeof (id) === 'undefined';

        if (!insertMode.current)
            api.get(`/customers/${id}`).then(response => { setCustomer({ ...response.data }) });
        else
            setCustomer({ ...customer, code: '', description: '', city: '' });
    }, [params]);

    const handleInputChange = (event) => {
        const { name, value } = event.target;
        const newCustomer = { ...customer, [name]: value };
        setCustomer({ ...newCustomer });
    };


    const handleSubmit = (event) => {
        event.preventDefault();
        if (!customer.id) {
            api
                .post(`/customers`, customer)
                .then(res => navigate('/'))
                .catch(err => {
                    setErrors(err.response.data.items)
                })
        } else {
            const id = customer.id;
            api
                .put(`/customers/${id}`, customer)
                .then(res => navigate('/'))
                .catch(err => {
                    setErrors(err.response.data.items)
                })
        }
    }

    const handleCancel = (event) => {
        navigate('/');
    }

    if (!customer)
        return <h1>Loading...</h1>
    return (
        <>
            <h3>Customer {insertMode.current === true ? 'create' : 'edit'}</h3>
            <form onSubmit={handleSubmit}>
                <div className="container">
                    {!insertMode.current &&
                        <div className="form-group">
                            <span>ID: {customer.id}</span>
                        </div>}

                    <div className="form-group">
                        <label htmlFor="inputCode">Code</label>
                        <input type="text" name="code" className="form-control" id="inputCode" placeholder="Enter Code" value={customer.code} onChange={handleInputChange} />
                    </div>

                    <div className="form-group">
                        <label htmlFor="inputDescription">Description</label>
                        <input type="text" name="description" className="form-control" id="inputDescription" placeholder="Enter Description" value={customer.description} onChange={handleInputChange} />
                    </div>

                    <div className="form-group">
                        <label htmlFor="inputCity">City</label>
                        <input type="text" name="city" className="form-control" id="inputCity" placeholder="Enter City" value={customer.city} onChange={handleInputChange} />
                    </div>

                    <div className="form-group">
                        <label htmlFor="inputNote">Note</label>
                        <textarea className="form-control" name="note" id="inputNote" rows="3" value={customer.note} onChange={handleInputChange} />
                    </div>

                    <div className="form-group">
                        <label htmlFor="inputRating">Rating</label>
                        <select value={customer.rating} name="rating" className="custom-select mr-sm-2" id="inputRating" onChange={handleInputChange} >
                            <option value="0">0</option>
                            <option value="1">1</option>
                            <option value="2">2</option>
                            <option value="3">3</option>
                            <option value="4">4</option>
                            <option value="5">5</option>
                        </select>

                    </div>
                    <Errors errors={errors} />
                    <div className="form-group">
                        <button type="submit" className="btn btn-primary">Save</button>
                        <button type="button" className="btn btn-danger" onClick={handleCancel}>Cancel</button>
                    </div>
                </div>
            </form>
        </>
    );
}

export default Customer;