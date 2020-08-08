import React from 'react';
import {BrowserRouter, Switch, Route} from 'react-router-dom';
import Customers from './components/customers';
import Customer from './components/customer';

const Routers =()=>(

    <BrowserRouter>
        <Switch>
            <Route exact path='/' component={Customers} />
            <Route path='/customer/:id' component={Customer} />
            <Route path='/customer' component={Customer} />
        </Switch>
    </BrowserRouter>

)

export default Routers;