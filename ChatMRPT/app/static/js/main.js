// Initialize when the DOM is fully loaded
document.addEventListener('DOMContentLoaded', function() {
    // DOM Elements
    const chatMessages = document.getElementById('chat-messages');
    const messageInput = document.getElementById('message-input');
    const sendButton = document.getElementById('send-message');
    const uploadButton = document.getElementById('upload-button');
    const downloadReportBtn = document.getElementById('download-report-btn');
    const generateReportBtn = document.getElementById('generate-report-btn');
    const languageSelector = document.getElementById('language-selector');
    const sessionStatusIndicator = document.getElementById('session-status-indicator');
    const statusDot = document.getElementById('status-dot');
    const statusTextElement = document.getElementById('status-text');
    const sidebarToggle = document.getElementById('sidebar-toggle');
    const sidebar = document.getElementById('app-sidebar');
    const closeSidebarBtn = document.getElementById('close-sidebar');

    // File upload elements
    const uploadFilesBtn = document.getElementById('upload-files-btn');
    const csvFileInput = document.getElementById('csv-upload');
    const shapefileInput = document.getElementById('shapefile-upload');
    const filesUploadStatus = document.getElementById('files-upload-status');

    // Sample Data Button (in modal)
    const useSampleDataBtnModal = document.getElementById('use-sample-data-btn-modal');

    // Bootstrap modals
    const uploadModalElem = document.getElementById('uploadModal');
    const uploadModal = uploadModalElem ? new bootstrap.Modal(uploadModalElem) : null;
    const reportModalElem = document.getElementById('reportModal');
    const reportModal = reportModalElem ? new bootstrap.Modal(reportModalElem) : null;
    const visualizationModalElem = document.getElementById('visualizationModal');
    const visualizationModal = visualizationModalElem ? new bootstrap.Modal(visualizationModalElem) : null;
    const visualizationModalBody = document.getElementById('visualizationModalBody');
    const visualizationModalLabel = document.getElementById('visualizationModalLabel');

    // App state
    let isWaitingForResponse = false;
    let sessionData = {
        csvLoaded: false,
        shapefileLoaded: false,
        analysisComplete: false,
        currentLanguage: 'en',
        currentCompositePage: 1,
        totalCompositePages: 1,
        currentBoxPlotPage: 1,
        totalBoxPlotPages: 1,
        boxPlotWardsPerPage: 20
    };

    // Initialize with welcome message
    addSystemMessage("Welcome to the MRPT AI Assistant. Let's analyze malaria risk!");
    addAssistantMessage(`
        <p><strong>Hello! I'm your Malaria Risk Assessment AI Assistant.</strong></p>
        <p>I can help you:</p>
        <ul>
            <li>Analyze malaria risk factors</li>
            <li>Create risk maps</li>
            <li>Identify vulnerable areas</li>
            <li>Prioritize resources effectively</li>
            <li>Generate detailed analysis reports</li>
        </ul>
        <p>To get started, upload your data files (CSV/Excel and Shapefile ZIP) using the upload button <i class="fas fa-upload"></i>, or <a href="#" id="use-sample-data-btn-initial" style="font-weight:bold; text-decoration: underline;">load sample data</a> to try out the tool.</p>
    `);

    // --- Event Listeners ---
    // Send message on button click or Enter key
    sendButton.addEventListener('click', sendMessage);
    messageInput.addEventListener('keypress', function(e) {
        if (e.key === 'Enter' && !e.shiftKey) {
            e.preventDefault();
            sendMessage();
        }
    });

    // Auto-resize message input textarea
    messageInput.addEventListener('input', function() {
        this.style.height = 'auto';
        this.style.height = Math.min(this.scrollHeight, 200) + 'px'; // Limit max height
    });

    // Show upload modal when upload button is clicked
    uploadButton.addEventListener('click', function() {
        if (uploadModal) uploadModal.show();
    });

    // Show report modal when download report button is clicked
    downloadReportBtn.addEventListener('click', function() {
        if (sessionData.analysisComplete) {
            if (reportModal) reportModal.show();
        } else {
            addSystemMessage("Please run an analysis before generating a report.");
        }
    });

    // Generate report when generate report button is clicked
    generateReportBtn.addEventListener('click', function() {
        const format = document.getElementById('report-format').value;
        generateReport(format);
        if (reportModal) reportModal.hide();
    });

    // Upload both files when upload files button is clicked
    uploadFilesBtn.addEventListener('click', function() {
        const csvFile = csvFileInput.files[0];
        const shapeFile = shapefileInput.files[0];
        
        if (!csvFile && !shapeFile) {
            filesUploadStatus.textContent = "Please select at least one file to upload";
            filesUploadStatus.className = "upload-status error";
            return;
        }
        
        uploadBothFiles(csvFile, shapeFile);
    });

    // Change language when language selector is changed
    languageSelector.addEventListener('change', function() {
        const newLanguage = this.value;
        if (newLanguage !== sessionData.currentLanguage) {
            changeLanguage(newLanguage);
        }
    });

    // Toggle sidebar
    if (sidebarToggle) {
        sidebarToggle.addEventListener('click', function(e) {
            e.preventDefault();
            if (sidebar) {
                sidebar.classList.toggle('open');
                document.body.classList.toggle('sidebar-open');
            }
        });
    }

    // Close sidebar when close button is clicked
    if (closeSidebarBtn) {
        closeSidebarBtn.addEventListener('click', function() {
            if (sidebar) {
                sidebar.classList.remove('open');
                document.body.classList.remove('sidebar-open');
            }
        });
    }

    // Close sidebar when clicking outside of it
    document.addEventListener('click', function(e) {
        if (sidebar && 
            sidebar.classList.contains('open') && 
            !sidebar.contains(e.target) && 
            e.target !== sidebarToggle && 
            !sidebarToggle.contains(e.target)) {
            sidebar.classList.remove('open');
            document.body.classList.remove('sidebar-open');
        }
    });

    // --- Event Listener using Delegation for dynamically added elements ---
    document.addEventListener('click', function(e) {
        // Sample Data link in initial message
        if (e.target && e.target.id === 'use-sample-data-btn-initial') {
             e.preventDefault();
             loadSampleData();
        }
        // Pagination controls
        else if (e.target.classList.contains('prev-composite')) {
            e.preventDefault(); navigateCompositeMap('prev', e);
        } else if (e.target.classList.contains('next-composite')) {
            e.preventDefault(); navigateCompositeMap('next', e);
        } else if (e.target.classList.contains('prev-boxplot')) {
            e.preventDefault(); navigateBoxPlot('prev', e);
        } else if (e.target.classList.contains('next-boxplot')) {
            e.preventDefault(); navigateBoxPlot('next', e);
        }
        // Expand Visualization Button
        else if (e.target.classList.contains('expand-visualization-btn') || e.target.closest('.expand-visualization-btn')) {
            e.preventDefault();
            const btn = e.target.classList.contains('expand-visualization-btn') ? e.target : e.target.closest('.expand-visualization-btn');
            const vizContainer = btn.closest('.visualization-container');
            if (vizContainer && visualizationModal) {
                const iframe = vizContainer.querySelector('iframe');
                const img = vizContainer.querySelector('img.viz-image');
                const titleElem = vizContainer.querySelector('.visualization-title');
                const title = titleElem ? titleElem.textContent : 'Visualization';

                visualizationModalLabel.textContent = title;
                visualizationModalBody.innerHTML = ''; // Clear previous content

                if (iframe) {
                    const newIframe = document.createElement('iframe');
                    newIframe.src = iframe.src;
                    newIframe.style.width = '100%';
                    newIframe.style.height = '100%';
                    newIframe.frameBorder = '0';
                    visualizationModalBody.appendChild(newIframe);
                } else if (img) {
                    const newImg = document.createElement('img');
                    newImg.src = img.src;
                    newImg.alt = title;
                    newImg.style.maxWidth = '100%';
                    newImg.style.maxHeight = '100%';
                    newImg.style.objectFit = 'contain';
                    visualizationModalBody.appendChild(newImg);
                }
                visualizationModal.show();
            }
        }
    });

    // Event listener for ward-per-page select boxes
    document.addEventListener('change', function(e) {
        if (e.target.classList.contains('wards-per-page')) {
            const newWardsPerPage = parseInt(e.target.value);
            if (!isNaN(newWardsPerPage) && newWardsPerPage !== sessionData.boxPlotWardsPerPage) {
                sessionData.boxPlotWardsPerPage = newWardsPerPage;
                const container = e.target.closest('.visualization-container');
                if (container) {
                    updateBoxPlotPagination(container, newWardsPerPage);
                }
            }
        }
    });

    // --- Function for Sample Data Button (in modal) ---
    if (useSampleDataBtnModal) {
        useSampleDataBtnModal.addEventListener('click', function(e) {
            e.preventDefault();
            loadSampleData();
            if (uploadModal) uploadModal.hide();
        });
    }

    // --- Core Functions ---
    function sendMessage() {
        const message = messageInput.value.trim();
        if (message === '' || isWaitingForResponse) return;

        // --- Check for sample data command ---
        const lowerMessage = message.toLowerCase();
        if (lowerMessage === 'load sample data' || lowerMessage === 'use sample data') {
            addUserMessage(message); // Show user command
            messageInput.value = '';
            messageInput.style.height = 'auto';
            loadSampleData(); // Call the sample data function
            return; // Stop further processing of this message
        }

        addUserMessage(message);
        messageInput.value = '';
        messageInput.style.height = 'auto';
        isWaitingForResponse = true;
        showTypingIndicator();

        // --- Check for different message types ---
        if (isVisualizationRequest(message)) {
            handleVisualizationRequest(message); // Pass original message
            return;
        }

        if (isRunAnalysisRequest(message)) {
            runAnalysis(); // runAnalysis handles indicators
            return;
        }

        // --- General message handling ---
        fetch('/send_message', {
            method: 'POST',
            headers: { 'Content-Type': 'application/json' },
            body: JSON.stringify({ message: message })
        })
        .then(response => response.json())
        .then(data => {
            hideTypingIndicator();
            addAssistantMessage(data.response);
            if (data.action) handleAction(data);
            isWaitingForResponse = false;
            scrollToBottom();
        })
        .catch(error => {
            console.error('Error sending message:', error);
            hideTypingIndicator();
            addSystemMessage("Error communicating with the server. Please try again.");
            isWaitingForResponse = false;
        });
    }

    // --- Upload both files simultaneously ---
    function uploadBothFiles(csvFile, shapeFile) {
        filesUploadStatus.innerHTML = '<div class="spinner-border spinner-border-sm text-primary" role="status"></div> Uploading files...';
        filesUploadStatus.className = "upload-status";
        showLoadingIndicator();
        
        // Create promises for each upload
        const promises = [];
        let csvResult = null;
        let shpResult = null;
        
        // Only upload CSV if provided
        if (csvFile) {
            const csvFormData = new FormData();
            csvFormData.append('file', csvFile);
            
            const csvPromise = fetch('/upload_csv', { method: 'POST', body: csvFormData })
                .then(response => response.json())
                .then(data => {
                    csvResult = data;
                    if (data.status === 'success') {
                        sessionData.csvLoaded = true;
                        sessionData.csvFilename = csvFile.name;
                    }
                    return data;
                });
            
            promises.push(csvPromise);
        }
        
        // Only upload shapefile if provided
        if (shapeFile) {
            const shpFormData = new FormData();
            shpFormData.append('file', shapeFile);
            
            const shpPromise = fetch('/upload_shapefile', { method: 'POST', body: shpFormData })
                .then(response => response.json())
                .then(data => {
                    shpResult = data;
                    if (data.status === 'success' || data.status === 'warning') {
                        sessionData.shapefileLoaded = true;
                        sessionData.shapefileFilename = shapeFile.name;
                    }
                    return data;
                });
            
            promises.push(shpPromise);
        }
        
        // When all uploads are complete
        Promise.all(promises)
            .then(() => {
                hideLoadingIndicator();
                let statusMessage = "";
                let statusClass = "success";
                
                // Handle CSV result
                if (csvResult) {
                    if (csvResult.status === 'success') {
                        statusMessage += `CSV file ${csvFile.name} uploaded successfully. `;
                        addSystemMessage(`<strong>CSV data loaded:</strong> ${csvFile.name} (Rows: ${csvResult.rows}, Columns: ${csvResult.columns})`);
                    } else {
                        statusMessage += `Error uploading CSV: ${csvResult.message}. `;
                        statusClass = "error";
                        addSystemMessage(`<strong>CSV Upload Error:</strong> ${csvResult.message}`);
                    }
                }
                
                // Handle shapefile result
                if (shpResult) {
                    if (shpResult.status === 'success') {
                        statusMessage += `Shapefile ${shapeFile.name} uploaded successfully. `;
                        addSystemMessage(`<strong>Shapefile loaded:</strong> ${shapeFile.name} (Features: ${shpResult.features})`);
                    } else if (shpResult.status === 'warning') {
                        statusMessage += `Shapefile ${shapeFile.name} uploaded with warnings: ${shpResult.message}. `;
                        statusClass = "warning";
                        addSystemMessage(`<strong>Shapefile loaded with warnings:</strong> ${shapeFile.name} (Features: ${shpResult.features}). ${shpResult.message}`);
                    } else {
                        statusMessage += `Error uploading shapefile: ${shpResult.message}. `;
                        statusClass = "error";
                        addSystemMessage(`<strong>Shapefile Upload Error:</strong> ${shpResult.message}`);
                    }
                }
                
                // Display overall status
                filesUploadStatus.textContent = statusMessage;
                filesUploadStatus.className = `upload-status ${statusClass}`;
                
                // Update session status
                updateSessionStatus();
                
                // Show analysis prompt if both files are loaded
                if (sessionData.csvLoaded && sessionData.shapefileLoaded) {
                    // If one of the uploads provided an analysis prompt, show it
                    if (csvResult && csvResult.analysis_prompt) {
                        addAssistantMessage(csvResult.analysis_prompt);
                    } else if (shpResult && shpResult.analysis_prompt) {
                        addAssistantMessage(shpResult.analysis_prompt);
                    } else {
                        addAssistantMessage("Both files loaded. You can now 'Run the analysis'.");
                    }
                    
                    // Close modal on success
                    if (uploadModal) uploadModal.hide();
                }
                
                scrollToBottom();
            })
            .catch(error => {
                hideLoadingIndicator();
                console.error('Error uploading files:', error);
                filesUploadStatus.textContent = "Error uploading files. Please try again.";
                filesUploadStatus.className = "upload-status error";
                addSystemMessage("<strong>Upload Error:</strong> Network or server issue.");
            });
    }

    // --- loadSampleData Function ---
    function loadSampleData() {
        if (isWaitingForResponse) return; // Prevent multiple clicks

        addSystemMessage("Loading sample data... Please wait.");
        isWaitingForResponse = true;
        showLoadingIndicator(); // Use global loading indicator

        fetch('/load_sample_data', { // Ensure this matches your Flask route
            method: 'POST',
            headers: {
                'Content-Type': 'application/json'
                // Add any other headers if needed (like CSRF tokens)
            }
        })
        .then(response => {
            if (!response.ok) {
                // Handle HTTP errors (like 500 Internal Server Error)
                return response.json().then(err => { throw new Error(err.message || `Server error: ${response.status}`) });
            }
            return response.json();
        })
        .then(data => {
            hideLoadingIndicator();
            isWaitingForResponse = false;

            if (data.status === 'success') {
                // Update frontend state
                sessionData.csvLoaded = true;
                sessionData.shapefileLoaded = true;
                sessionData.analysisComplete = false; // Reset analysis state
                updateSessionStatus();

                // Provide feedback in chat
                addSystemMessage(`Sample CSV loaded successfully (Rows: ${data.rows}, Columns: ${data.columns}).`);
                addSystemMessage(`Sample Shapefile loaded successfully (Features: ${data.features}).`);

                // Display the prompt from the backend
                if (data.analysis_prompt) {
                    addAssistantMessage(data.analysis_prompt);
                } else {
                     addAssistantMessage("Sample data is loaded. You can now type 'Run the analysis'.");
                }
                scrollToBottom();

            } else {
                addSystemMessage(`<strong>Error loading sample data:</strong><br>${data.message || 'Unknown error occurred.'}`);
            }
        })
        .catch(error => {
            hideLoadingIndicator();
            isWaitingForResponse = false;
            console.error('Error loading sample data:', error);
            addSystemMessage(`<strong>Error loading sample data:</strong><br>${error.message || 'Could not connect to the server.'}`);
        });
    }

    // --- Check for analysis request messages ---
    function isRunAnalysisRequest(message) {
        const lowerMsg = message.toLowerCase();
        const directPatterns = [
            /^run(?:\s+the)?\s+analysis$/i, /^analyze(?:\s+the)?\s+data$/i,
            /^start(?:\s+the)?\s+analysis$/i, /^process(?:\s+the)?\s+data$/i,
            /^begin(?:\s+the)?\s+analysis$/i
        ];
        return directPatterns.some(pattern => pattern.test(lowerMsg));
    }

    // --- Check for visualization request messages ---
    function isVisualizationRequest(message) {
        const lowerMsg = message.toLowerCase();
        const vizWords = ['show', 'display', 'view', 'see', 'generate', 'create', 'draw', 'make', 'visualize', 'plot', 'map'];
        const vizTypes = ['map', 'plot', 'visualization', 'chart', 'tree', 'graph', 'figure'];
        const hasVizVerb = vizWords.some(word => lowerMsg.includes(word));
        const hasVizType = vizTypes.some(type => lowerMsg.includes(type));
        const specificTypes = [
            'variable map', 'normalized map', 'composite map', 'vulnerability map',
            'urban extent', 'box plot', 'whisker plot', 'vulnerability plot',
            'decision tree', 'ranking plot' // Added ranking plot alias
        ];
        const hasSpecificType = specificTypes.some(type => lowerMsg.includes(type));
        // Trigger if it clearly asks for a specific type or uses a viz verb + general type noun
        return hasSpecificType || (hasVizVerb && hasVizType);
    }

    // --- Handle visualization requests ---
    // Enhanced visualization request function with retry logic
    function handleVisualizationRequest(message) {
        const vizInfo = extractVisualizationInfo(message); // Use the improved extraction function
        if (!vizInfo.type) {
            // If we couldn't determine a specific type, fall back to the AI
            console.log("Couldn't extract specific visualization type, falling back to AI.");
            return sendMessageToBackend(message);
        }

        // Show loading indicator for visualization
        showLoadingIndicator();
        addSystemMessage(`<div class="pending-viz-message">
            <div class="spinner-border spinner-border-sm text-primary" role="status"></div>
            Generating ${getVisualizationTypeName(vizInfo.type)}${vizInfo.threshold > 0 ? ` at ${vizInfo.threshold}% threshold` : ''}... Please wait.
        </div>`);

        // Make the request with retry logic
        requestVisualizationWithRetry(vizInfo, 0);
    }

    // Helper to get friendly visualization name
    function getVisualizationTypeName(vizType) {
        switch(vizType) {
            case 'variable_map': return 'variable map';
            case 'normalized_map': return 'normalized map';
            case 'composite_map': return 'composite map';
            case 'vulnerability_map': return 'vulnerability map';
            case 'urban_extent_map': 
                return 'urban extent and vulnerability map';
            case 'vulnerability_plot': return 'vulnerability ranking plot';
            case 'decision_tree': return 'analysis workflow diagram';
            default: return 'visualization';
        }
    }

    // Retry mechanism for visualization requests
    function requestVisualizationWithRetry(vizInfo, attemptCount, maxAttempts = 2) {
        fetch('/get_visualization', {
            method: 'POST',
            headers: { 'Content-Type': 'application/json' },
            body: JSON.stringify(vizInfo)
        })
        .then(response => {
            // Check if response is ok before parsing
            if (!response.ok) {
                throw new Error(`HTTP error! status: ${response.status}`);
            }
            return response.json();
        })
        .then(data => {
            hideLoadingIndicator();
            isWaitingForResponse = false;
            
            // Remove the pending visualization message if it exists
            const pendingVizMessages = document.querySelectorAll('.pending-viz-message');
            pendingVizMessages.forEach(el => el.parentElement.remove());
            
            if (data.status === 'success') {
                // Update session to track visualization for context
                sessionData.lastVisualization = {
                    type: vizInfo.type,
                    variable: vizInfo.variable,
                    threshold: vizInfo.threshold,
                    timestamp: new Date().toISOString()
                };
                
                // Show the visualization
                const title = getVisualizationTitle(data.viz_type || vizInfo.type, data.variable || vizInfo.variable, vizInfo.threshold);
                addVisualization(data.image_path, title, data.viz_type || vizInfo.type, data);
                
                // Display AI explanation if provided
                if (data.ai_response) {
                    addAssistantMessage(data.ai_response);
                }
            } else {
                // Check if we should retry
                if (attemptCount < maxAttempts) {
                    console.log(`Visualization request failed, retrying (${attemptCount + 1}/${maxAttempts})...`);
                    // Wait a bit longer before retrying
                    setTimeout(() => {
                        requestVisualizationWithRetry(vizInfo, attemptCount + 1, maxAttempts);
                    }, 1000 * (attemptCount + 1)); // Increasing delay with each retry
                    return;
                }
                
                // After max retries, show error
                addSystemMessage(`<strong>Error generating visualization:</strong><br>${data.message || 'Could not generate visualization'}`);
                if (data.ai_response) {
                    addAssistantMessage(data.ai_response);
                }
            }
            scrollToBottom();
        })
        .catch(error => {
            console.error('Error getting visualization:', error);
            
            // Check if we should retry
            if (attemptCount < maxAttempts) {
                console.log(`Visualization request failed, retrying (${attemptCount + 1}/${maxAttempts})...`);
                // Wait a bit longer before retrying
                setTimeout(() => {
                    requestVisualizationWithRetry(vizInfo, attemptCount + 1, maxAttempts);
                }, 1000 * (attemptCount + 1)); // Increasing delay with each retry
                return;
            }
            
            // After max retries, clean up and show error
            hideLoadingIndicator();
            isWaitingForResponse = false;
            hideTypingIndicator();
            
            // Remove the pending visualization message if it exists
            const pendingVizMessages = document.querySelectorAll('.pending-viz-message');
            pendingVizMessages.forEach(el => el.parentElement.remove());
            
            addSystemMessage(`<strong>Error requesting visualization:</strong><br>The server encountered an issue. Please try again or use different wording.`);
            scrollToBottom();
        });
    }

    // Update the getVisualizationTitle function to include threshold
    function getVisualizationTitle(vizType, variableName = null, threshold = 0) {
        let title = 'Visualization'; // Default
        const fullVarName = variableName ? getFullVariableNameToDisplay(variableName) : 'Selected Variable';

        switch (vizType) {
            case 'variable_map': title = `Variable: ${fullVarName}`; break;
            case 'normalized_map': title = `Normalized: ${fullVarName}`; break;
            case 'composite_map': title = 'Composite Risk Maps'; break;
            case 'vulnerability_map': title = 'Ward Vulnerability Map'; break;
            case 'vulnerability_plot': title = 'Ward Vulnerability Ranking'; break;
            case 'urban_extent_map': 
                if (threshold <= 0) {
                    title = 'Ward Vulnerability Map'; // Standard vulnerability map
                } else {
                    title = `Urban Extent (${threshold}%) & Vulnerability Map`; // Combined map
                }
                break;
            case 'decision_tree': title = 'Analysis Workflow'; break;
        }
        return title;
    }

    // Helper function to send message to backend AI
    function sendMessageToBackend(message) {
        fetch('/send_message', {
            method: 'POST',
            headers: { 'Content-Type': 'application/json' },
            body: JSON.stringify({ message: message })
        })
        .then(response => response.json())
        .then(data => {
            hideTypingIndicator();
            addAssistantMessage(data.response);
            if (data.action) handleAction(data);
            isWaitingForResponse = false;
            scrollToBottom();
        })
        .catch(error => {
            console.error('Error sending message:', error);
            hideTypingIndicator();
            addSystemMessage("Error communicating with the server. Please try again.");
            isWaitingForResponse = false;
        });
    }

    // --- Extract visualization information from message ---
    // Improved extraction function for visualization requests
    function extractVisualizationInfo(message) {
        const lowerMsg = message.toLowerCase();
        const vizInfo = { 
            type: null, 
            variable: null, 
            threshold: 0 // Default to 0% for integrated urban/vulnerability map
        };

        // Process urban threshold patterns first - more comprehensive pattern matching
        // Look for various ways users might specify a threshold
        const thresholdPatterns = [
            /(\d+(?:\.\d+)?)\s*%/i,                                  // "50%"
            /threshold\s+(?:of\s+)?(\d+(?:\.\d+)?)/i,                // "threshold of 50"
            /at\s+(\d+(?:\.\d+)?)\s*(?:percent|%)?/i,                // "at 50%" or "at 50"
            /with\s+(\d+(?:\.\d+)?)\s*(?:%|percent)/i,               // "with 50%" or "with 50 percent"
            /(\d+(?:\.\d+)?)\s*(?:percent|%)\s+(?:urban|threshold)/i, // "50% urban" or "50% threshold"
            /(\d+(?:\.\d+)?)\s*threshold/i,                          // "50 threshold"
            /(?:show|display).*?(\d+(?:\.\d+)?)/i                    // "show...at 50" - last resort
        ];

        // Try each threshold pattern
        for (const pattern of thresholdPatterns) {
            const match = lowerMsg.match(pattern);
            if (match && match[1]) {
                const threshold = parseFloat(match[1]);
                // Validate range (0-100%)
                vizInfo.threshold = Math.max(0, Math.min(100, threshold));
                break; // Stop after first match
            }
        }

        // Now check for map type - prioritize specific patterns
        // Urban extent and vulnerability map (combined)
        if (
            (lowerMsg.includes('urban') && lowerMsg.includes('extent') && lowerMsg.includes('map')) ||
            (lowerMsg.includes('urban') && lowerMsg.includes('threshold')) ||
            (lowerMsg.includes('urban') && lowerMsg.includes('vulnerab')) || 
            // When both urban and vulnerability are mentioned, it's likely requesting the integrated map
            (lowerMsg.includes('vulnerab') && lowerMsg.includes('threshold')) || 
            (lowerMsg.includes('vulnerab') && lowerMsg.includes('urban'))
        ) {
            vizInfo.type = 'urban_extent_map';
            // Threshold was already extracted above
        }
        // Pure vulnerability map request
        else if (
            (lowerMsg.includes('vulnerab') && lowerMsg.includes('map') && !lowerMsg.includes('urban')) ||
            (lowerMsg.includes('vulnerab') && lowerMsg.includes('rank') && !lowerMsg.includes('urban'))
        ) {
            // If it's just a vulnerability map request without mentioning urban/threshold
            // Set type to urban_extent_map with threshold 0 (which will show regular vulnerability map)
            vizInfo.type = 'urban_extent_map';
            vizInfo.threshold = 0; // Force to 0 to show regular vulnerability map
        }
        // Other map types
        else if ((lowerMsg.includes('variable') || lowerMsg.includes('distribution')) && lowerMsg.includes('map')) {
            vizInfo.type = 'variable_map';
            vizInfo.variable = extractVariable(message);
        } else if (lowerMsg.includes('normalized') && lowerMsg.includes('map')) {
            vizInfo.type = 'normalized_map';
            vizInfo.variable = extractVariable(message);
        } else if (lowerMsg.includes('composite') && lowerMsg.includes('map')) {
            vizInfo.type = 'composite_map';
        } else if (lowerMsg.includes('decision') && lowerMsg.includes('tree')) {
            vizInfo.type = 'decision_tree';
        } else if (
            (lowerMsg.includes('box') && lowerMsg.includes('plot')) || 
            (lowerMsg.includes('box') && lowerMsg.includes('whisker')) ||
            ((lowerMsg.includes('vulnerab') || lowerMsg.includes('rank')) && lowerMsg.includes('plot'))
        ) {
            vizInfo.type = 'vulnerability_plot';
        }
        // Generic map request with a variable
        else if (lowerMsg.includes('map')) {
            const extractedVar = extractVariable(message);
            if (extractedVar) {
                vizInfo.type = 'variable_map'; 
                vizInfo.variable = extractedVar;
            } else {
                // Default to composite map if no variable is mentioned
                vizInfo.type = 'composite_map';
            }
        }
        // Generic plot request
        else if (lowerMsg.includes('plot') || lowerMsg.includes('chart') || lowerMsg.includes('graph')) {
            vizInfo.type = 'vulnerability_plot';
        }

        console.log(`Extracted visualization info: type=${vizInfo.type}, variable=${vizInfo.variable}, threshold=${vizInfo.threshold}`);
        return vizInfo;
    }

    // --- Extract variable name from message ---
    function extractVariable(message) {
       // Improved extraction - look for nouns after prepositions or specific keywords
       const lowerMsg = message.toLowerCase();
       let potentialVar = null;

       // Patterns like "map of X", "plot for Y", "show Z map"
       const patterns = [
           /(?:map|plot|chart|graph|visualization|distribution)\s+(?:of|for|about)\s+([\w\s_]+)/i,
           /(?:show|display|view|visualize)\s+([\w\s_]+)\s+(?:map|plot|chart|graph|distribution)/i,
            /(?:show|display|view|visualize)\s+([\w\s_]+)/i // Less specific, check last
       ];

       for (const pattern of patterns) {
           const match = message.match(pattern);
           if (match && match[1]) {
                const candidate = match[1].trim().toLowerCase();
                // Avoid common non-variable words if caught by broad patterns
                const stopWords = ['me', 'the', 'a', 'an', 'my', 'data', 'map', 'plot', 'chart', 'graph', 'visualization', 'distribution'];
                if (!stopWords.includes(candidate) && candidate.length > 2) { // Basic filtering
                   potentialVar = candidate.replace(/\s+/g, '_'); // Normalize spaces to underscores
                   break; // Found a likely candidate
                }
           }
       }

       // Check for common variable names
       if (!potentialVar) {
           const commonVariables = [ // Order more specific ones first
               'distance_to_water', 'mean_rainfall', 'mean_soil_wetness', 'mean_evi', 'mean_ndvi',
               'mean_ndwi', 'housing_quality', 'temp_mean', 'rh_mean', 'settlement_type', 'u5_tpr_rdt',
               'urbanPercentage', 'building_height', 'pfpr', 'rainfall', 'temperature', 'elevation',
               'population', 'ndvi', 'evi', 'ndwi', 'flood'
           ];
            for (const variable of commonVariables) {
                // Use word boundaries to avoid partial matches within other words
                const regex = new RegExp(`\\b${variable.replace('_', '[_\\s]?')}\\b`, 'i');
                if (regex.test(lowerMsg)) {
                    potentialVar = variable;
                    break;
                }
            }
        }

       console.log(`Extracted variable candidate: ${potentialVar} from "${message}"`);
       return potentialVar; // Return the normalized name or null
    }

    // --- Extract threshold from message ---
    function extractThreshold(message) {
        // Default threshold
        const defaultThreshold = 0; // Changed to 0 for urban/vulnerability integration
        
        // Convert message to lowercase for case-insensitive matching
        const messageLower = message.toLowerCase();
        
        // Pattern 1: Find "X%" pattern (e.g., "50%", "75%")
        const percentMatch = messageLower.match(/(\d+(?:\.\d+)?)\s*%/);
        if (percentMatch && percentMatch[1]) {
            const threshold = parseFloat(percentMatch[1]);
            // Validate range (0-100%)
            return Math.max(0, Math.min(100, threshold));
        }
        
        // Pattern 2: Look for "threshold of X" or "at X" or "X threshold" or "X percent"
        const patterns = [
            /threshold\s+(?:of\s+)?(\d+(?:\.\d+)?)/i,  // "threshold of 50" or "threshold 50"
            /at\s+(\d+(?:\.\d+)?)\s*(?:percent|%)?/i,   // "at 50%" or "at 50 percent" or just "at 50"
            /set\s+to\s+(\d+(?:\.\d+)?)/i,             // "set to 50"
            /level\s+(?:of\s+)?(\d+(?:\.\d+)?)/i,      // "level of 50" or "level 50"
            /(\d+(?:\.\d+)?)\s*(?:percent|pct|%)/i,    // "50 percent" or "50%"
            /(\d+(?:\.\d+)?)\s*threshold/i,            // "50 threshold"
            /(\d+(?:\.\d+)?)\s*urban/i,                // "50 urban"
            /show\s+(?:.*?\s+)?(\d+(?:\.\d+)?)/i,      // "show at 50" or "show map at 50"
            /(\d+(?:\.\d+)?)/                          // Last resort - just find any number
        ];
        
        for (const pattern of patterns) {
            const match = messageLower.match(pattern);
            if (match && match[1]) {
                const threshold = parseFloat(match[1]);
                // Validate range (0-100%)
                return Math.max(0, Math.min(100, threshold));
            }
        }
        
        // Return default if no patterns match
        return defaultThreshold;
    }

    // --- Run Analysis ---
    function runAnalysis() {
       // Check if data is loaded
        if (!sessionData.csvLoaded || !sessionData.shapefileLoaded) {
           addSystemMessage("Please load both the CSV/Excel and Shapefile (ZIP) before running the analysis.");
           hideTypingIndicator(); // Hide if shown by sendMessage
           isWaitingForResponse = false; // Reset state
           return;
       }
       addSystemMessage("<strong>Running standard analysis...</strong> This may take a few moments.");
       isWaitingForResponse = true; // Set waiting state
       showLoadingIndicator(); showTypingIndicator(); // Show indicators

       fetch('/run_analysis', { method: 'POST', headers: { 'Content-Type': 'application/json' }, body: JSON.stringify({}) })
       .then(response => response.json())
       .then(data => {
           hideLoadingIndicator(); hideTypingIndicator();
           isWaitingForResponse = false; // Reset waiting state
           if (data.status === 'success') {
               sessionData.analysisComplete = true; updateSessionStatus();
               const varsUsed = data.variables_used || [];
               const topWards = data.vulnerable_wards || [];
               addSystemMessage(`<strong>Analysis complete!</strong><br>Used ${varsUsed.length} variables.<br>Top vulnerable: ${topWards.slice(0, 3).join(', ')}${topWards.length > 3 ? ', ...' : ''}`);
               addAssistantMessage(analysisSuccessResponseMessage(data, false)); // Pass false for standard
           } else {
               addSystemMessage(`<strong>Error running analysis</strong><br>${data.message || 'Unknown error'}`);
           }
            scrollToBottom();
       })
       .catch(error => {
           console.error('Error running analysis:', error);
           hideLoadingIndicator(); hideTypingIndicator();
           isWaitingForResponse = false; // Reset waiting state
           addSystemMessage("<strong>Error running analysis</strong><br>Could not connect to the server. Please try again.");
       });
    }

    // --- Generate success response after analysis ---
    function analysisSuccessResponseMessage(data, isCustom) {
       const customText = isCustom ? "with the variables you specified" : "using default parameters";
       const varsUsed = data.variables_used || [];
       const topWards = data.vulnerable_wards || [];
       return `
           <p><strong>${isCustom ? "Custom a" : "A"}nalysis completed successfully!</strong></p>
           <p>I've analyzed your data ${customText}. Key results:</p>
           <ul>
               <li><strong>Variables Used:</strong> ${varsUsed.length > 0 ? varsUsed.join(', ') : 'Default set'}</li>
               <li><strong>Top 5 Vulnerable Wards:</strong> ${topWards.length > 0 ? topWards.join(', ') : 'N/A'}</li>
           </ul>
           <p>You can now ask me to show you visualizations like:</p>
           <ul>
               <li>"Show map for population" (Variable Map)</li>
               <li>"Show normalized map for rainfall"</li>
               <li>"Show composite map"</li>
               <li>"Show vulnerability plot" (Ranking)</li>
               <li>"Show vulnerability map"</li>
               <li>"Show urban extent map at 50%"</li>
               <li>"Show decision tree"</li>
           </ul>
           <p>Or <a href="#" onclick="document.getElementById('download-report-btn').click(); return false;">generate a report</a>. What would you like to see first?</p>
       `;
    }

    // --- Generate Report ---
    function generateReport(format) {
        if (!sessionData.analysisComplete) {
           addSystemMessage("Please run the analysis before generating a report.");
           return;
       }
       addSystemMessage(`<strong>Generating ${format.toUpperCase()} report...</strong>`);
       isWaitingForResponse = true;
       showLoadingIndicator(); showTypingIndicator();

       // Use the send_message endpoint to trigger report generation
       fetch('/send_message', {
           method: 'POST', headers: { 'Content-Type': 'application/json' },
           body: JSON.stringify({ message: `Generate ${format} report` }) // Use a message the backend understands
       })
       .then(response => response.json())
       .then(data => {
           hideLoadingIndicator(); hideTypingIndicator();
           isWaitingForResponse = false;
           if (data.action === 'show_report' && data.report_url) {
                addSystemMessage(`<strong>Report generated!</strong><br><br><a href="${data.report_url}" class="btn btn-success" download target="_blank"><i class="fas fa-download"></i> Download ${format.toUpperCase()} Report</a>`);
                if(data.response) addAssistantMessage(data.response); // Show AI confirmation
                // Trigger download automatically (optional)
                // setTimeout(() => { window.open(data.report_url, '_blank'); }, 500);
           } else {
               addSystemMessage(`<strong>Error generating report:</strong><br>${data.response || data.message || 'Unknown error'}`);
           }
            scrollToBottom();
       }).catch(error => {
           console.error('Error generating report:', error);
           hideLoadingIndicator(); hideTypingIndicator();
            isWaitingForResponse = false;
           addSystemMessage("<strong>Error generating report:</strong><br>Could not connect to the server.");
       });
    }

    // --- Change Language ---
    function changeLanguage(language) {
       languageSelector.value = language; // Update dropdown
       sessionData.currentLanguage = language;
       addSystemMessage(`Changing language to ${getLanguageName(language)}...`);
       isWaitingForResponse = true;
       showTypingIndicator();

       fetch('/send_message', {
           method: 'POST', headers: { 'Content-Type': 'application/json' },
           body: JSON.stringify({ message: `Change language to ${getLanguageName(language)}` }) // Send human-readable name
       })
       .then(response => response.json())
       .then(data => {
           hideTypingIndicator();
            isWaitingForResponse = false;
            if(data.status === 'success' || data.action === 'language_changed'){
                // Backend might already send a system message, or we can add one
                // addSystemMessage(`Language changed to ${getLanguageName(language)}.`);
                if(data.response) addAssistantMessage(data.response);
            } else {
                addSystemMessage(`<strong>Error changing language:</strong> ${data.response || data.message || 'Unknown error'}`);
                languageSelector.value = sessionData.currentLanguage; // Revert dropdown on error
            }
            scrollToBottom();
       }).catch(error => {
           console.error('Error changing language:', error);
           hideTypingIndicator();
           isWaitingForResponse = false;
           addSystemMessage("<strong>Error changing language:</strong> Could not connect to server.");
           languageSelector.value = sessionData.currentLanguage; // Revert dropdown on error
       });
    }

    // --- Handle Actions from Backend ---
    function handleAction(data) {
       // Actions triggered by the backend /send_message response
       switch(data.action) {
           case 'analysis_complete':
           case 'analysis_updated': // Handle potential update action
               sessionData.analysisComplete = true; updateSessionStatus();
               // Response message is handled by sendMessage caller
               break;
           case 'show_visualization':
               if (data.visualization && data.viz_type) {
                   const title = getVisualizationTitle(data.viz_type, data.variable);
                   addVisualization(data.visualization, title, data.viz_type, data); // Pass full data
               } else {
                    addSystemMessage("Backend requested visualization but didn't provide necessary details.");
               }
               break;
           case 'show_report':
                if (data.report_url) {
                   const format = data.report_url.split('.').pop().toUpperCase();
                    addSystemMessage(`<strong>Report generated!</strong><br><br><a href="${data.report_url}" class="btn btn-success" download target="_blank"><i class="fas fa-download"></i> Download ${format} Report</a>`);
               } else {
                   addSystemMessage("Backend indicated report ready, but URL missing.");
               }
               break;
           case 'language_changed':
               // Update state if necessary, message already handled
               sessionData.currentLanguage = languageSelector.value;
               break;
           case 'error':
                addSystemMessage(`<strong>Error from backend:</strong><br>${data.message || data.response || 'An unspecified error occurred'}`);
               break;
           default:
               console.log("Received unhandled action:", data.action);
       }
    }

    // --- Update Session Status ---
    function updateSessionStatus() {
        let currentStatusText = "Ready";
        let currentDotClass = "status-dot ready"; // Default (gray)

        if (sessionData.analysisComplete) {
            currentStatusText = "Analysis Complete";
            currentDotClass = "status-dot analysis-complete"; // Green
        } else if (sessionData.csvLoaded && sessionData.shapefileLoaded) {
            currentStatusText = "Data Loaded";
            currentDotClass = "status-dot data-loaded"; // Blue
        } else if (sessionData.csvLoaded || sessionData.shapefileLoaded) {
            currentStatusText = sessionData.csvLoaded ? "CSV Loaded" : "Shapefile Loaded";
            // Keep blue if one file is loaded, but not fully ready for analysis
            currentDotClass = "status-dot data-loaded";
        }

        if (statusTextElement) {
            statusTextElement.textContent = currentStatusText;
        }
        if (statusDot) {
            statusDot.className = currentDotClass;
        }
    }

    // --- Add Visualization ---
    function addVisualization(vizPath, title, vizType, vizData = {}) {
        const messageDiv = document.createElement('div');
        // Add 'visualization-message' class to override max-width constraints
        messageDiv.className = 'message assistant-message visualization-message new-message'; 
        const contentDiv = document.createElement('div');
        contentDiv.className = 'message-content';
        const vizContainer = document.createElement('div');
        vizContainer.className = 'visualization-container';
        vizContainer.style.position = 'relative'; // Ensure relative positioning for button

        const titleElement = document.createElement('h4');
        titleElement.className = 'visualization-title';
        titleElement.textContent = title;
        vizContainer.appendChild(titleElement);

        // Expand Button
        const expandButton = document.createElement('button');
        expandButton.className = 'btn btn-sm btn-outline-secondary expand-visualization-btn';
        expandButton.innerHTML = '<i class="fas fa-expand-alt"></i> View Larger';
        expandButton.style.position = 'absolute';
        expandButton.style.top = '10px'; // Adjust position as needed
        expandButton.style.right = '10px';
        expandButton.title = 'View larger';
        vizContainer.appendChild(expandButton);

        if (vizPath.endsWith('.html')) {
            const iframe = document.createElement('iframe');
            iframe.src = vizPath + '?t=' + Date.now(); // Cache busting
            iframe.width = '100%';
            // Increased heights for better visibility
            iframe.height = (vizType === 'composite_map' || vizType === 'vulnerability_plot') ? '600px' : '500px';
            iframe.frameBorder = '0';
            iframe.style.borderRadius = '8px';
            iframe.style.boxShadow = '0 2px 5px rgba(0,0,0,0.1)';
            // Add minimum dimensions to ensure adequate size
            iframe.style.minWidth = '600px';
            iframe.style.minHeight = (vizType === 'composite_map' || vizType === 'vulnerability_plot') ? '550px' : '450px';
            iframe.onerror = () => vizContainer.innerHTML += '<p class="text-danger">Error loading visualization.</p>';
            vizContainer.appendChild(iframe);
        } else if (vizPath.endsWith('.png') || vizPath.endsWith('.jpg') || vizPath.endsWith('.jpeg') || vizPath.endsWith('.svg')) { // Handle image paths
            const img = document.createElement('img');
            img.src = vizPath + '?t=' + Date.now(); // Cache busting
            img.className = 'viz-image img-fluid'; // Use img-fluid for responsiveness
            img.alt = title;
            img.style.borderRadius = '8px';
            img.style.boxShadow = '0 2px 5px rgba(0,0,0,0.1)';
            // Set minimum width for images as well
            img.style.minWidth = '600px';
            img.onerror = () => vizContainer.innerHTML += '<p class="text-danger">Error loading image.</p>';
            vizContainer.appendChild(img);
        } else {
            vizContainer.innerHTML += '<p class="text-warning">Unsupported visualization format.</p>';
        }

        // Pagination controls
        if (vizType === 'composite_map' && vizData.total_pages > 1) {
            sessionData.currentCompositePage = vizData.current_page || 1;
            sessionData.totalCompositePages = vizData.total_pages || 1;
            addCompositePaginationControls(vizContainer);
        } else if (vizType === 'vulnerability_plot' && vizData.total_pages > 1) {
            sessionData.currentBoxPlotPage = vizData.current_page || 1;
            sessionData.totalBoxPlotPages = vizData.total_pages || 1;
            // Use wards_per_page from vizData if available, else use sessionData
            sessionData.boxPlotWardsPerPage = vizData.wards_per_page || sessionData.boxPlotWardsPerPage;
            addBoxPlotPaginationControls(vizContainer);
        }

        contentDiv.appendChild(vizContainer);
        messageDiv.appendChild(contentDiv);
        chatMessages.appendChild(messageDiv);
        scrollToBottom();
    }

    // --- Add Composite Map Pagination Controls ---
    function addCompositePaginationControls(container) {
        const paginationDiv = document.createElement('div');
        paginationDiv.className = 'pagination-controls text-center mt-2';

        const prevButton = document.createElement('button');
        prevButton.innerHTML = '<i class="fas fa-arrow-left"></i> Previous';
        prevButton.className = 'btn btn-outline-primary btn-sm prev-composite me-2';
        prevButton.disabled = sessionData.currentCompositePage <= 1;

        const pageInfo = document.createElement('span');
        pageInfo.className = 'pagination-info align-middle';
        pageInfo.textContent = `Page ${sessionData.currentCompositePage} of ${sessionData.totalCompositePages}`;

        const nextButton = document.createElement('button');
        nextButton.innerHTML = 'Next <i class="fas fa-arrow-right"></i>';
        nextButton.className = 'btn btn-outline-primary btn-sm next-composite ms-2';
        nextButton.disabled = sessionData.currentCompositePage >= sessionData.totalCompositePages;

        paginationDiv.appendChild(prevButton);
        paginationDiv.appendChild(pageInfo);
        paginationDiv.appendChild(nextButton);
        container.appendChild(paginationDiv);
    }

    // --- Add Box Plot Pagination Controls ---
    function addBoxPlotPaginationControls(container) {
        const paginationDiv = document.createElement('div');
        paginationDiv.className = 'pagination-controls text-center mt-3';

        const prevButton = document.createElement('button');
        prevButton.innerHTML = '<i class="fas fa-arrow-left"></i> Previous';
        prevButton.className = 'btn btn-outline-primary btn-sm prev-boxplot me-2';
        prevButton.disabled = sessionData.currentBoxPlotPage <= 1;

        const pageInfo = document.createElement('span');
        pageInfo.className = 'pagination-info align-middle mx-2'; // Added horizontal margin
        pageInfo.textContent = `Page ${sessionData.currentBoxPlotPage} of ${sessionData.totalBoxPlotPages}`;

        const nextButton = document.createElement('button');
        nextButton.innerHTML = 'Next <i class="fas fa-arrow-right"></i>';
        nextButton.className = 'btn btn-outline-primary btn-sm next-boxplot ms-2';
        nextButton.disabled = sessionData.currentBoxPlotPage >= sessionData.totalBoxPlotPages;

        const wardsPerPageDiv = document.createElement('div');
        wardsPerPageDiv.className = 'mt-2 wards-per-page-container d-inline-block ms-3'; // Inline display
        const wardsPerPageLabel = document.createElement('label');
        const uniqueSelectId = 'wards-per-page-select-' + Date.now(); // Ensure unique ID
        wardsPerPageLabel.textContent = 'Wards/Page: ';
        wardsPerPageLabel.htmlFor = uniqueSelectId;
        wardsPerPageLabel.className = 'form-label me-1 mb-0 align-middle'; // Align middle

        const wardsPerPageSelect = document.createElement('select');
        wardsPerPageSelect.id = uniqueSelectId;
        wardsPerPageSelect.className = 'form-select form-select-sm d-inline-block wards-per-page align-middle';
        wardsPerPageSelect.style.width = 'auto';

        [10, 15, 20, 25, 30].forEach(num => {
            const option = document.createElement('option');
            option.value = num;
            option.textContent = num;
            // Set selected based on current sessionData state
            if (num === sessionData.boxPlotWardsPerPage) option.selected = true;
            wardsPerPageSelect.appendChild(option);
        });

        wardsPerPageDiv.appendChild(wardsPerPageLabel);
        wardsPerPageDiv.appendChild(wardsPerPageSelect);

        paginationDiv.appendChild(prevButton);
        paginationDiv.appendChild(pageInfo);
        paginationDiv.appendChild(nextButton);
        paginationDiv.appendChild(wardsPerPageDiv); // Add wards per page selector
        container.appendChild(paginationDiv);
    }

    // --- Update Box Plot Pagination ---
    function updateBoxPlotPagination(container, newWardsPerPage) {
        showLoadingIndicator();
        fetch('/update_boxplot_pagination', { // Ensure route exists
            method: 'POST',
            headers: { 'Content-Type': 'application/json' },
            body: JSON.stringify({
                wards_per_page: newWardsPerPage
            })
        })
        .then(response => response.json())
        .then(data => {
            hideLoadingIndicator();
            if (data.status === 'success') {
                const iframe = container.querySelector('iframe');
                if (iframe) iframe.src = data.image_path + '?t=' + Date.now(); // Reload iframe with cache buster

                sessionData.currentBoxPlotPage = data.current_page;
                sessionData.totalBoxPlotPages = data.total_pages;
                sessionData.boxPlotWardsPerPage = newWardsPerPage; // Update session state

                // Update pagination controls within this specific container
                const paginationInfo = container.querySelector('.pagination-info');
                if (paginationInfo) paginationInfo.textContent = `Page ${data.current_page} of ${data.total_pages}`;

                const prevBtn = container.querySelector('.prev-boxplot');
                if (prevBtn) prevBtn.disabled = data.current_page <= 1;

                const nextBtn = container.querySelector('.next-boxplot');
                if (nextBtn) nextBtn.disabled = data.current_page >= data.total_pages;

                // Ensure the select dropdown reflects the current value
                const select = container.querySelector('.wards-per-page');
                if (select) select.value = newWardsPerPage;

            } else {
                addSystemMessage(`<strong>Error updating plot</strong><br>${data.message || 'Error updating wards per page.'}`);
                // Revert select value if update failed? Optional.
                const select = container.querySelector('.wards-per-page');
                if (select) select.value = sessionData.boxPlotWardsPerPage; // Revert to previous value
            }
        })
        .catch(error => {
            hideLoadingIndicator();
            console.error('Error updating wards per page:', error);
            addSystemMessage("<strong>Error updating plot</strong><br>Could not update wards per page. Please try again.");
            // Revert select value on error
            const select = container.querySelector('.wards-per-page');
            if (select) select.value = sessionData.boxPlotWardsPerPage; // Revert to previous value
        });
    }

    // --- Navigate Composite Map ---
    function navigateCompositeMap(direction, event) {
        const vizContainer = event.target.closest('.visualization-container');
        if (!vizContainer) return;

        showLoadingIndicator();
        fetch('/navigate_composite_map', {
            method: 'POST', headers: { 'Content-Type': 'application/json' },
            body: JSON.stringify({ direction: direction, current_page: sessionData.currentCompositePage })
        })
        .then(response => response.json())
        .then(data => {
            hideLoadingIndicator();
            if (data.status === 'success') {
                sessionData.currentCompositePage = data.current_page;
                sessionData.totalCompositePages = data.total_pages; // Ensure this is updated

                const iframe = vizContainer.querySelector('iframe');
                if (iframe) iframe.src = data.image_path + '?t=' + Date.now(); // Update iframe src with cache buster

                const paginationInfo = vizContainer.querySelector('.pagination-info');
                if (paginationInfo) paginationInfo.textContent = `Page ${data.current_page} of ${data.total_pages}`;

                const prevButton = vizContainer.querySelector('.prev-composite');
                if (prevButton) prevButton.disabled = data.current_page <= 1;

                const nextButton = vizContainer.querySelector('.next-composite');
                if (nextButton) nextButton.disabled = data.current_page >= data.total_pages;

            } else {
                addSystemMessage(`<strong>Map Navigation Error:</strong><br>${data.message || 'Error navigating maps'}`);
            }
        }).catch(error => {
            hideLoadingIndicator();
            console.error('Error navigating composite maps:', error);
            addSystemMessage(`<strong>Map Navigation Error:</strong><br>Could not navigate. Please try again.`);
        });
    }

    // --- Navigate Box Plot ---
    function navigateBoxPlot(direction, event) {
        const vizContainer = event.target.closest('.visualization-container');
        if (!vizContainer) return;
        showLoadingIndicator();

        fetch('/navigate_boxplot', { // Ensure this route exists and handles pagination
            method: 'POST', headers: { 'Content-Type': 'application/json' },
            body: JSON.stringify({
                direction: direction,
                current_page: sessionData.currentBoxPlotPage,
                // wards_per_page: sessionData.boxPlotWardsPerPage // Send if needed by backend
            })
        })
        .then(response => response.json())
        .then(data => {
            hideLoadingIndicator();
            if (data.status === 'success') {
                sessionData.currentBoxPlotPage = data.current_page;
                sessionData.totalBoxPlotPages = data.total_pages;

                const iframe = vizContainer.querySelector('iframe');
                if (iframe) iframe.src = data.image_path + '?t=' + Date.now(); // Update iframe src

                const paginationInfo = vizContainer.querySelector('.pagination-info');
                if (paginationInfo) paginationInfo.textContent = `Page ${data.current_page} of ${data.total_pages}`;

                const prevButton = vizContainer.querySelector('.prev-boxplot');
                if (prevButton) prevButton.disabled = data.current_page <= 1;

                const nextButton = vizContainer.querySelector('.next-boxplot');
                if (nextButton) nextButton.disabled = data.current_page >= data.total_pages;

            } else {
                addSystemMessage(`<strong>Plot Navigation Error:</strong><br>${data.message || 'Error navigating plots'}`);
            }
        }).catch(error => {
            hideLoadingIndicator();
            console.error('Error navigating box plots:', error);
            addSystemMessage(`<strong>Plot Navigation Error:</strong><br>Could not navigate. Please try again.`);
        });
    }

    // --- Get Language Name ---
    function getLanguageName(code) {
        const languages = {'en':'English','ha':'Hausa','yo':'Yoruba','ig':'Igbo','ff':'Fulfulde','kr':'Kanuri','fr':'French','ar':'Arabic'};
        return languages[code] || 'English'; // Default to English
    }

    // --- Get Visualization Title ---
    function getVisualizationTitle(vizType, variableName = null) {
        let title = 'Visualization'; // Default
        const fullVarName = variableName ? getFullVariableNameToDisplay(variableName) : 'Selected Variable';

        switch (vizType) {
            case 'variable_map': title = `Variable: ${fullVarName}`; break;
            case 'normalized_map': title = `Normalized: ${fullVarName}`; break;
            case 'composite_map': title = 'Composite Risk Maps'; break;
            case 'vulnerability_map': title = 'Ward Vulnerability Map'; break;
            case 'vulnerability_plot': title = 'Ward Vulnerability Ranking'; break;
            case 'urban_extent_map': title = `Urban Extent & Vulnerability Map (${vizData.threshold || 0}%)`; break;
            case 'decision_tree': title = 'Analysis Workflow'; break;
        }
        return title;
    }

    // Helper to get more readable variable names for titles
    function getFullVariableNameToDisplay(variableName) {
        if (!variableName) return 'Variable';
        // Simple replacements and capitalization
        return variableName
            .replace(/_/g, ' ')
            .replace(/\b\w/g, char => char.toUpperCase()); // Capitalize first letter of each word
    }

    // --- UI Helper Functions ---
    function addUserMessage(message) {
        const messageDiv = document.createElement('div');
        messageDiv.className = 'message user-message new-message'; // Added new-message
        // Use escapeHTML for user input to prevent XSS
        messageDiv.innerHTML = `<div class="message-content">${escapeHTML(message)}</div>`;
        chatMessages.appendChild(messageDiv); scrollToBottom();
    }

    function addAssistantMessage(message) {
        const messageDiv = document.createElement('div');
        messageDiv.className = 'message assistant-message new-message'; // Added new-message
        // Allow HTML from assistant (assuming it's trusted or sanitized backend-side)
        // Directly sets the innerHTML with the raw message from the backend
        messageDiv.innerHTML = `<div class="message-content">${message}</div>`; 
        chatMessages.appendChild(messageDiv); 
        scrollToBottom();
    }

    function addSystemMessage(message) {
        const messageDiv = document.createElement('div');
        messageDiv.className = 'message system-message new-message'; // Added new-message
        messageDiv.innerHTML = message; // Allow HTML for system messages (e.g., buttons, links)
        chatMessages.appendChild(messageDiv); scrollToBottom();
    }

    function showTypingIndicator() {
        hideTypingIndicator(); // Remove existing if any
        const indicatorDiv = document.createElement('div');
        indicatorDiv.className = 'message assistant-message typing-indicator'; // Style like assistant message
        indicatorDiv.id = 'typing-indicator';
        indicatorDiv.innerHTML = '<div class="message-content"><span></span><span></span><span></span></div>';
        chatMessages.appendChild(indicatorDiv); scrollToBottom();
    }

    function hideTypingIndicator() {
        const indicator = document.getElementById('typing-indicator');
        if (indicator) indicator.remove();
    }

    // Global loading indicator overlay
    function showLoadingIndicator() {
        if (!document.getElementById('global-loading-indicator')) {
            const loadingDiv = document.createElement('div');
            loadingDiv.id = 'global-loading-indicator';
            loadingDiv.className = 'loading-indicator-overlay';
            loadingDiv.innerHTML = '<div class="spinner-border text-light" role="status"></div><p>Processing...</p>';
            document.body.appendChild(loadingDiv);
        }
    }

    function hideLoadingIndicator() {
        const loadingDiv = document.getElementById('global-loading-indicator');
        if (loadingDiv) loadingDiv.remove();
    }

    function scrollToBottom() {
        // Add a small delay to allow the DOM to update heights after adding a message
        setTimeout(() => {
            chatMessages.scrollTop = chatMessages.scrollHeight;
        }, 50);
    }

    // Basic HTML escaping
    function escapeHTML(str) {
        if (!str) return '';
        const div = document.createElement('div');
        div.textContent = str;
        return div.innerHTML;
    }

    // Initial status update
    updateSessionStatus();

    // Accept and acknowledge cookie policy
    const cookieAcceptButton = document.getElementById('accept-cookies');
    const cookieBanner = document.getElementById('cookie-banner');
    
    if (cookieAcceptButton && cookieBanner) {
        cookieAcceptButton.addEventListener('click', function() {
            // Set cookie to remember user accepted
            document.cookie = "mrpt_cookies_accepted=true; max-age=31536000; path=/";
            cookieBanner.style.display = 'none';
        });
        
        // Check if user already accepted cookies
        if (document.cookie.indexOf('mrpt_cookies_accepted=true') !== -1) {
            cookieBanner.style.display = 'none';
        }
    }
});