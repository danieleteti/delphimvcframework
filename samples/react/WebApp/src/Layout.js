import React from 'react';
import { BrowserRouter, Routes, Route, Outlet, Link } from 'react-router-dom';
import Customers from './components/customers/Customers';
import Customer from './components/customer/Customer';
import About from './components/about/About';

const Holder = () => {
    return <div>
        <nav className="navbar navbar-light bg-light justify-content-between">
            <span>🧭 React DelphiMVCFramework Sample</span>
            <ul>
                <li >
                    <Link to={'/customer'}>Add</Link>
                </li>
                <li >
                    <Link to={'/about'}>About</Link>
                </li>
            </ul>
        </nav>
        <Outlet></Outlet>
    </div>
}

const Layout = () => {
    return <BrowserRouter>
        <Routes>
            <Route path='/' element={<Holder></Holder>} >
                <Route index path='/' element={<Customers></Customers>} />
                <Route path='/customer/:id' element={<Customer></Customer>} />
                <Route path='/customer' element={<Customer></Customer>} />
                <Route path='/about' element={<About></About>} />
                <Route path='*' element={<div>Not Found</div>} />
            </Route>
        </Routes>
    </BrowserRouter>
};

export default Layout;