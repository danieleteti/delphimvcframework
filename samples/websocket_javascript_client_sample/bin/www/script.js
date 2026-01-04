// ***************************************************************************
//
// Delphi MVC Framework
//
// Copyright (c) 2010-2025 Daniele Teti and the DMVCFramework Team
//
// https://github.com/danieleteti/delphimvcframework
//
// ***************************************************************************
//
// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// You may obtain a copy of the License at
//
// http://www.apache.org/licenses/LICENSE-2.0
//
// Unless required by applicable law or agreed to in writing, software
// distributed under the License is distributed on an "AS IS" BASIS,
// WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
// See the License for the specific language governing permissions and
// limitations under the License.
//
// ***************************************************************************

// WebSocket Client for DMVCFramework Demo
// ITDevCon 2025 - Real-Time Stock Monitor Demo

class WebSocketMonitor {
    constructor() {
        this.ws = null;
        this.reconnectTimeout = null;
        this.stats = {
            totalMessages: 0,
            echoMessages: 0,
            heartbeatMessages: 0,
            latencies: [],
            connectionStart: null
        };

        // Available stocks (will be populated by server)
        this.availableStocks = [];
        this.updateInterval = null; // Will be set by server

        // Stock data for chart (keep last 30 data points)
        this.stockData = {};
        this.maxPoints = 30;

        // Animation state for smooth transitions
        this.animationState = {};
        this.animationProgress = 1;
        this.animationDuration = 0; // Will be calculated from updateInterval

        this.lastUpdateTime = Date.now();

        // DOM Elements
        this.statusDot = document.getElementById('statusDot');
        this.statusText = document.getElementById('statusText');
        this.messageCount = document.getElementById('messageCount');
        this.heartbeatCount = document.getElementById('heartbeatCount');
        this.messageLog = document.getElementById('messageLog');
        this.latencyElement = document.getElementById('latency');
        this.uptimeElement = document.getElementById('uptime');
        this.stockSelector = document.getElementById('stockSelector');
        this.updateIntervalElement = document.getElementById('updateInterval');

        // Initialize canvas chart
        this.canvas = document.getElementById('stockChart');
        this.ctx = this.canvas.getContext('2d');
        this.initializeCanvas();

        // Bind button events
        document.getElementById('sendBtn').addEventListener('click', () => this.sendTestMessage());
        document.getElementById('clearBtn').addEventListener('click', () => this.clearLog());
        document.getElementById('reconnectBtn').addEventListener('click', () => this.reconnect());

        // Connect
        this.connect();

        // Update uptime every second
        setInterval(() => this.updateUptime(), 1000);

        // Animation loop for smooth chart updates
        this.animateChart();
    }

    initializeCanvas() {
        // Set canvas size with proper pixel ratio
        const container = this.canvas.parentElement;
        const dpr = window.devicePixelRatio || 1;

        console.log('Container dimensions:', container.offsetWidth, 'x', container.offsetHeight);

        this.canvas.width = container.offsetWidth * dpr;
        this.canvas.height = container.offsetHeight * dpr;
        this.canvas.style.width = container.offsetWidth + 'px';
        this.canvas.style.height = container.offsetHeight + 'px';

        console.log('Canvas dimensions:', this.canvas.width, 'x', this.canvas.height);
        console.log('Canvas style:', this.canvas.style.width, 'x', this.canvas.style.height);

        this.ctx.scale(dpr, dpr);

        // Draw initial placeholder
        this.drawChart();

        // Handle window resize
        window.addEventListener('resize', () => {
            const newDpr = window.devicePixelRatio || 1;
            this.canvas.width = container.offsetWidth * newDpr;
            this.canvas.height = container.offsetHeight * newDpr;
            this.canvas.style.width = container.offsetWidth + 'px';
            this.canvas.style.height = container.offsetHeight + 'px';
            this.ctx.scale(newDpr, newDpr);
            this.drawChart();
        });
    }

    connect() {
        this.log('[System] Connecting to ws://localhost:9091...', 'system');

        try {
            this.ws = new WebSocket('ws://localhost:9091');

            this.ws.onopen = () => this.onOpen();
            this.ws.onmessage = (event) => this.onMessage(event);
            this.ws.onerror = (error) => this.onError(error);
            this.ws.onclose = (event) => this.onClose(event);

        } catch (error) {
            console.error('WebSocket creation error:', error);
            this.log(`[Error] Connection failed: ${error.message}`, 'error');
            this.scheduleReconnect();
        }
    }

    onOpen() {
        this.log('[System] ✓ Connected to WebSocket server!', 'system');
        this.updateConnectionStatus(true);
        this.stats.connectionStart = Date.now();

        // Clear reconnect timeout if exists
        if (this.reconnectTimeout) {
            clearTimeout(this.reconnectTimeout);
            this.reconnectTimeout = null;
        }
    }

    onMessage(event) {
        const message = event.data;

        this.stats.totalMessages++;

        // Try to parse as JSON (stock data or stock list)
        try {
            const data = JSON.parse(message);

            if (data.type === 'stocklist') {
                // Initialize stock list from server
                this.handleStockList(data.stocks, data.updateInterval);
                this.log('[System] Received stock list from server', 'system');
            } else if (data.type === 'stocks') {
                this.handleStockData(data);
                this.stats.heartbeatMessages++;
                this.animateMetric(this.heartbeatCount, this.stats.heartbeatMessages);

                // Log only active stocks
                const activeStocks = this.availableStocks.filter(s => s.active);
                const stockPrices = activeStocks.map(s => `${s.symbol}=$${data[s.symbol].toFixed(2)}`).join(' ');
                this.log(`[Stocks] ${stockPrices}`, 'heartbeat');

                // Simulate latency
                const latency = Math.floor(Math.random() * 50) + 10;
                this.stats.latencies.push(latency);
                if (this.stats.latencies.length > 10) {
                    this.stats.latencies.shift();
                }
                this.updateLatency();
            }
        } catch (e) {
            // Not JSON, handle as text message
            if (message.includes('Echo:')) {
                this.stats.echoMessages++;
                this.log(`[Echo] ${message}`, 'echo');
            } else {
                this.log(`[Server] ${message}`, 'server');
            }
        }

        // Update counters
        this.animateMetric(this.messageCount, this.stats.totalMessages);
    }

    handleStockList(stocks, updateInterval) {
        console.log('Received stock list:', stocks);
        console.log('Update interval from server:', updateInterval, 'ms');

        // Store update interval from server
        this.updateInterval = updateInterval;
        // Calculate animation duration (slightly less than update interval for smooth transition)
        this.animationDuration = Math.max(updateInterval * 0.9, 100);

        // Update UI with server's update interval
        if (this.updateIntervalElement) {
            const seconds = (updateInterval / 1000).toFixed(1);
            this.updateIntervalElement.textContent = `⚡ ${seconds} second${seconds !== '1.0' ? 's' : ''}`;
        }

        // Initialize available stocks from server
        this.availableStocks = stocks.map((stock, index) => ({
            ...stock,
            active: index < 3 // First 3 stocks active by default
        }));

        console.log('Initialized stocks:', this.availableStocks);

        // Initialize data structures
        this.availableStocks.forEach(stock => {
            this.stockData[stock.symbol] = [];
            this.animationState[stock.symbol] = [];
        });

        // Build stock selector UI
        this.buildStockSelector();

        // Send active stocks list to server
        this.sendActiveStocksList();
    }

    sendActiveStocksList() {
        if (this.ws && this.ws.readyState === WebSocket.OPEN) {
            const activeSymbols = this.availableStocks
                .filter(s => s.active)
                .map(s => s.symbol);

            const message = JSON.stringify({
                type: 'subscribe',
                symbols: activeSymbols
            });

            console.log('Sending active stocks to server:', activeSymbols);
            this.ws.send(message);
        }
    }

    buildStockSelector() {
        this.stockSelector.innerHTML = '';

        this.availableStocks.forEach(stock => {
            const item = document.createElement('div');
            item.className = 'stock-item' + (stock.active ? ' active' : '');
            item.innerHTML = `
                <div class="stock-info">
                    <span class="stock-color-indicator" style="background: ${stock.color};"></span>
                    <span class="stock-symbol">${stock.symbol}</span>
                    <span class="stock-name">${stock.name}</span>
                </div>
                <span class="stock-value" data-symbol="${stock.symbol}">$0.00</span>
            `;

            item.addEventListener('click', () => this.toggleStock(stock.symbol));
            this.stockSelector.appendChild(item);
        });
    }

    toggleStock(symbol) {
        const stock = this.availableStocks.find(s => s.symbol === symbol);
        if (stock) {
            stock.active = !stock.active;

            // Update UI
            const item = Array.from(this.stockSelector.children).find(
                el => el.querySelector(`[data-symbol="${symbol}"]`)
            );
            if (item) {
                if (stock.active) {
                    item.classList.add('active');
                } else {
                    item.classList.remove('active');
                }
            }

            this.log(`[User] ${stock.active ? 'Activated' : 'Deactivated'} ${stock.name} (${symbol})`, 'system');

            // Send updated list to server
            this.sendActiveStocksList();
        }
    }

    handleStockData(data) {
        // Store current values as starting point for animation
        this.availableStocks.forEach(stock => {
            if (this.stockData[stock.symbol]) {
                this.animationState[stock.symbol] = [...this.stockData[stock.symbol]];
            }
        });

        // Add new data points for all stocks
        this.availableStocks.forEach(stock => {
            const symbol = stock.symbol;
            if (data[symbol] !== undefined) {
                this.stockData[symbol].push(data[symbol]);

                // Keep only last N points
                if (this.stockData[symbol].length > this.maxPoints) {
                    this.stockData[symbol].shift();

                    if (this.animationState[symbol] && this.animationState[symbol].length > 0) {
                        this.animationState[symbol].shift();
                    }
                }

                // Update price display
                const priceElement = this.stockSelector.querySelector(`[data-symbol="${symbol}"]`);
                if (priceElement) {
                    priceElement.textContent = `$${data[symbol].toFixed(2)}`;
                }
            }
        });

        // Start animation
        this.animationProgress = 0;
        this.lastUpdateTime = Date.now();
    }

    // Easing function for smooth animation
    easeInOutCubic(t) {
        return t < 0.5 ? 4 * t * t * t : 1 - Math.pow(-2 * t + 2, 3) / 2;
    }

    // Interpolate between two arrays
    interpolateData(from, to, progress) {
        const result = [];
        const maxLen = Math.max(from.length, to.length);

        for (let i = 0; i < maxLen; i++) {
            const fromVal = from[i] || to[i] || 0;
            const toVal = to[i] || fromVal || 0;
            result.push(fromVal + (toVal - fromVal) * progress);
        }

        return result;
    }

    // Animation loop
    animateChart() {
        // Update animation progress
        if (this.animationProgress < 1) {
            const elapsed = Date.now() - this.lastUpdateTime;
            this.animationProgress = Math.min(elapsed / this.animationDuration, 1);
        }

        // Draw chart with current animation state
        this.drawChart();

        // Continue animation loop
        requestAnimationFrame(() => this.animateChart());
    }

    drawChart() {
        const ctx = this.ctx;
        const width = this.canvas.style.width ? parseInt(this.canvas.style.width) : this.canvas.width;
        const height = this.canvas.style.height ? parseInt(this.canvas.style.height) : this.canvas.height;
        const paddingLeft = 70;
        const paddingRight = 20;
        const paddingTop = 20;
        const paddingBottom = 30;

        // Clear canvas
        ctx.clearRect(0, 0, width, height);

        // Check if stocks are loaded
        if (!this.availableStocks || this.availableStocks.length === 0) {
            // Draw placeholder text with better visibility
            ctx.fillStyle = 'rgba(255,255,255,0.8)';
            ctx.font = 'bold 18px Arial';
            ctx.textAlign = 'center';
            ctx.textBaseline = 'middle';
            ctx.fillText('📊 Waiting for connection...', width / 2, height / 2 - 10);
            ctx.font = '14px Arial';
            ctx.fillStyle = 'rgba(255,255,255,0.6)';
            ctx.fillText('(Connecting to WebSocket server)', width / 2, height / 2 + 15);
            return;
        }

        // Get only active stocks
        const activeStocks = this.availableStocks.filter(s => s.active);
        if (activeStocks.length === 0) {
            ctx.fillStyle = 'rgba(255,255,255,0.8)';
            ctx.font = 'bold 18px Arial';
            ctx.textAlign = 'center';
            ctx.textBaseline = 'middle';
            ctx.fillText('📊 Select stocks from the list to display', width / 2, height / 2);
            return;
        }

        // Calculate interpolated data for smooth animation
        const easedProgress = this.easeInOutCubic(this.animationProgress);
        const interpolatedData = {};

        activeStocks.forEach(stock => {
            interpolatedData[stock.symbol] = this.interpolateData(
                this.animationState[stock.symbol] || [],
                this.stockData[stock.symbol] || [],
                easedProgress
            );
        });

        // Find min and max values across all active stocks
        const allValues = [];
        Object.values(interpolatedData).forEach(data => allValues.push(...data));

        if (allValues.length === 0) {
            return;
        }

        const minValue = Math.min(...allValues);
        const maxValue = Math.max(...allValues);
        const valueRange = maxValue - minValue || 1;

        // Draw grid lines
        ctx.strokeStyle = 'rgba(255,255,255,0.1)';
        ctx.lineWidth = 1;
        for (let i = 0; i < 5; i++) {
            const y = paddingTop + (height - paddingTop - paddingBottom) * i / 4;
            ctx.beginPath();
            ctx.moveTo(paddingLeft, y);
            ctx.lineTo(width - paddingRight, y);
            ctx.stroke();

            // Draw value labels
            const value = maxValue - (valueRange * i / 4);
            ctx.fillStyle = 'rgba(255,255,255,0.9)';
            ctx.font = 'bold 14px Arial';
            ctx.textAlign = 'right';
            ctx.fillText(`$${value.toFixed(0)}`, paddingLeft - 10, y + 5);
        }

        // Helper function to draw line
        const drawLine = (data, color) => {
            if (data.length < 2) {
                return;
            }

            const stepX = (width - paddingLeft - paddingRight) / (this.maxPoints - 1);

            // Draw shadow first
            ctx.strokeStyle = 'rgba(0,0,0,0.3)';
            ctx.lineWidth = 4;
            ctx.shadowBlur = 8;
            ctx.shadowColor = 'rgba(0,0,0,0.4)';
            ctx.shadowOffsetY = 2;
            ctx.beginPath();

            data.forEach((value, index) => {
                const x = paddingLeft + stepX * index;
                const y = height - paddingBottom - ((value - minValue) / valueRange) * (height - paddingTop - paddingBottom);
                if (index === 0) {
                    ctx.moveTo(x, y);
                } else {
                    ctx.lineTo(x, y);
                }
            });

            ctx.stroke();

            // Reset shadow and draw main line
            ctx.shadowBlur = 0;
            ctx.shadowOffsetY = 0;
            ctx.strokeStyle = color;
            ctx.lineWidth = 3;
            ctx.beginPath();

            data.forEach((value, index) => {
                const x = paddingLeft + stepX * index;
                const y = height - paddingBottom - ((value - minValue) / valueRange) * (height - paddingTop - paddingBottom);
                if (index === 0) {
                    ctx.moveTo(x, y);
                } else {
                    ctx.lineTo(x, y);
                }
            });

            ctx.stroke();
        };

        // Draw lines for each active stock using interpolated data
        activeStocks.forEach(stock => {
            drawLine(interpolatedData[stock.symbol], stock.color);
        });
    }

    onError(error) {
        this.log(`[Error] WebSocket error occurred`, 'error');
        console.error('WebSocket error:', error);
    }

    onClose(event) {
        this.log(`[System] Connection closed. Attempting to reconnect...`, 'system');
        this.updateConnectionStatus(false);
        this.scheduleReconnect();
    }

    scheduleReconnect() {
        if (!this.reconnectTimeout) {
            this.reconnectTimeout = setTimeout(() => {
                this.log('[System] Reconnecting...', 'system');
                this.connect();
            }, 3000);
        }
    }

    reconnect() {
        if (this.ws) {
            this.ws.close();
        }
        this.connect();
    }

    sendTestMessage() {
        if (this.ws && this.ws.readyState === WebSocket.OPEN) {
            const messages = [
                'Hello from ITDevCon 2025!',
                'WebSocket push is awesome!',
                'Real-time communication rocks!',
                'DMVCFramework is powerful!',
                'No polling needed!'
            ];
            const randomMessage = messages[Math.floor(Math.random() * messages.length)];

            this.ws.send(randomMessage);
            this.log(`[Sent] ${randomMessage}`, 'sent');
        } else {
            this.log('[Error] Cannot send - not connected', 'error');
        }
    }

    updateConnectionStatus(connected) {
        if (connected) {
            this.statusDot.classList.add('connected');
            this.statusText.textContent = 'Connected';
        } else {
            this.statusDot.classList.remove('connected');
            this.statusText.textContent = 'Disconnected';
        }
    }

    log(message, type = 'info') {
        const entry = document.createElement('div');
        entry.className = `log-entry ${type}`;

        const timestamp = new Date().toLocaleTimeString();
        entry.innerHTML = `<span class="timestamp">[${timestamp}]</span> ${message}`;

        this.messageLog.appendChild(entry);
        this.messageLog.scrollTop = this.messageLog.scrollHeight;

        // Keep only last 100 entries
        while (this.messageLog.children.length > 100) {
            this.messageLog.removeChild(this.messageLog.firstChild);
        }
    }

    clearLog() {
        this.messageLog.innerHTML = '<div class="log-entry"><span class="timestamp">[System]</span> Log cleared</div>';
    }

    animateMetric(element, value) {
        element.classList.add('update');
        element.textContent = value;
        setTimeout(() => element.classList.remove('update'), 300);
    }

    updateLatency() {
        if (this.stats.latencies.length > 0) {
            const avg = Math.round(
                this.stats.latencies.reduce((a, b) => a + b, 0) / this.stats.latencies.length
            );
            this.latencyElement.textContent = `${avg}ms`;
        }
    }

    updateUptime() {
        if (this.stats.connectionStart) {
            const uptime = Math.floor((Date.now() - this.stats.connectionStart) / 1000);
            const minutes = Math.floor(uptime / 60);
            const seconds = uptime % 60;
            if (minutes > 0) {
                this.uptimeElement.textContent = `${minutes}m ${seconds}s`;
            } else {
                this.uptimeElement.textContent = `${seconds}s`;
            }
        }
    }

    destroy() {
        if (this.ws) {
            this.ws.close();
        }
    }
}

// Initialize the monitor when page loads
let monitor;
window.addEventListener('DOMContentLoaded', () => {
    monitor = new WebSocketMonitor();
});

// Cleanup on page unload
window.addEventListener('beforeunload', () => {
    if (monitor) {
        monitor.destroy();
    }
});
