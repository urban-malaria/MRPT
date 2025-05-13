// Initialize when the DOM is fully loaded
document.addEventListener('DOMContentLoaded', function() {
    // DOM Elements
    const chatMessages = document.getElementById('chat-messages');
    const messageInput = document.getElementById('message-input');
    const sendButton = document.getElementById('send-message');
    const uploadButton = document.getElementById('upload-button');
    const downloadReportBtn = document.getElementById('download-report-btn');
    const generateReportBtn = document.getElementById('generate-report-btn');
    // const reportDownloadLink = document.getElementById('report-download-link'); // Not found in HTML, likely unused
    const languageSelector = document.getElementById('language-selector');
    // Corrected selectors for session status
    const sessionStatusIndicator = document.getElementById('session-status-indicator');
    const statusDot = document.getElementById('status-dot');
    const statusTextElement = document.getElementById('status-text');


    // File upload elements
    const uploadCsvBtn = document.getElementById('upload-csv-btn');
    const uploadShapefileBtn = document.getElementById('upload-shapefile-btn');
    const csvFileInput = document.getElementById('csv-upload');
    const shapefileInput = document.getElementById('shapefile-upload');
    const csvUploadStatus = document.getElementById('csv-upload-status');
    const shapefileUploadStatus = document.getElementById('shapefile-upload-status');

    // Sample Data Button (in modal)
    const useSampleDataBtnModal = document.getElementById('use-sample-data-btn-modal'); // Added

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

    addSystemMessage("Welcome to the MRPT AI Assistant. Let's analyze malaria risk!");
    // Initial message including sample data option (link generated here)
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

    sendButton.addEventListener('click', sendMessage);

    messageInput.addEventListener('keypress', function(e) {
        if (e.key === 'Enter' && !e.shiftKey) {
            e.preventDefault();
            sendMessage();
        }
    });

    messageInput.addEventListener('input', function() {
        this.style.height = 'auto';
        this.style.height = Math.min(this.scrollHeight, 200) + 'px'; // Limit max height
    });

    uploadButton.addEventListener('click', function() {
        if (uploadModal) uploadModal.show();
    });

    downloadReportBtn.addEventListener('click', function() {
        if (sessionData.analysisComplete) {
            if (reportModal) reportModal.show();
        } else {
            addSystemMessage("Please run an analysis before generating a report.");
        }
    });

    generateReportBtn.addEventListener('click', function() {
        const format = document.getElementById('report-format').value;
        generateReport(format);
        if (reportModal) reportModal.hide();
    });

    uploadCsvBtn.addEventListener('click', function() {
        const file = csvFileInput.files[0];
        if (file) {
            uploadCSV(file);
        } else {
            csvUploadStatus.textContent = "Please select a file first";
            csvUploadStatus.className = "upload-status error";
        }
    });

    uploadShapefileBtn.addEventListener('click', function() {
        const file = shapefileInput.files[0];
        if (file) {
            uploadShapefile(file);
        } else {
            shapefileUploadStatus.textContent = "Please select a file first";
            shapefileUploadStatus.className = "upload-status error";
        }
    });

    languageSelector.addEventListener('change', function() {
        const newLanguage = this.value;
        if (newLanguage !== sessionData.currentLanguage) {
            changeLanguage(newLanguage);
        }
    });

    // --- Event Listener for Sample Data Button (in modal) ---
    if (useSampleDataBtnModal) {
        useSampleDataBtnModal.addEventListener('click', function(e) {
            e.preventDefault();
            loadSampleData();
            if (uploadModal) uploadModal.hide();
        });
    }

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
        // --- End of check ---

        addUserMessage(message);
        messageInput.value = '';
        messageInput.style.height = 'auto';
        isWaitingForResponse = true;
        showTypingIndicator();

        // --- Existing logic (visualization, run analysis, general message) ---
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


    function isRunAnalysisRequest(message) {
        const lowerMsg = message.toLowerCase();
        const directPatterns = [
            /^run(?:\s+the)?\s+analysis$/i, /^analyze(?:\s+the)?\s+data$/i,
            /^start(?:\s+the)?\s+analysis$/i, /^process(?:\s+the)?\s+data$/i,
            /^begin(?:\s+the)?\s+analysis$/i
        ];
        return directPatterns.some(pattern => pattern.test(lowerMsg));
    }

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


    function handleVisualizationRequest(message) {
        const vizInfo = extractVisualizationInfo(message); // Use the function that extracts type, variable, threshold
        if (!vizInfo.type) {
            // If we couldn't determine a specific type, fall back to the AI
            console.log("Couldn't extract specific visualization type, falling back to AI.");
            fetch('/send_message', {
                method: 'POST',
                headers: { 'Content-Type': 'application/json' },
                body: JSON.stringify({ message: message }) // Send the original message
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
                console.error('Error sending fallback message:', error);
                hideTypingIndicator();
                addSystemMessage("Error communicating with the server. Please try again.");
                isWaitingForResponse = false;
            });
            return;
        }

        // If type WAS extracted, proceed to fetch visualization
        console.log("Requesting visualization:", vizInfo);
        fetch('/get_visualization', {
            method: 'POST',
            headers: { 'Content-Type': 'application/json' },
            body: JSON.stringify(vizInfo)
        })
        .then(response => response.json())
        .then(data => {
            hideTypingIndicator();
            if (data.status === 'success') {
                const title = getVisualizationTitle(data.viz_type || vizInfo.type, data.variable || vizInfo.variable); // Use returned type/var if available
                addVisualization(data.image_path, title, data.viz_type || vizInfo.type, data); // Pass full data for pagination
                if (data.ai_response) {
                    addAssistantMessage(data.ai_response); // Display AI explanation if provided
                }
            } else {
                addSystemMessage(`Error: ${data.message || 'Could not generate visualization'}`);
                if (data.ai_response) { // Show AI response even on error
                    addAssistantMessage(data.ai_response);
                }
            }
            isWaitingForResponse = false;
            scrollToBottom();
        })
        .catch(error => {
            console.error('Error getting visualization:', error);
            hideTypingIndicator();
            addSystemMessage("Error requesting visualization from the server. Please try again.");
            isWaitingForResponse = false;
        });
    }


    function extractVisualizationInfo(message) {
        const lowerMsg = message.toLowerCase();
        const vizInfo = { type: null, variable: null, threshold: 30 }; // Initialize

        // Prioritize specific plot types
        if (lowerMsg.includes('decision tree') || lowerMsg.includes('workflow') || (lowerMsg.includes('tree') && !lowerMsg.includes('map'))) {
            vizInfo.type = 'decision_tree';
        } else if (lowerMsg.includes('box plot') || lowerMsg.includes('whisker plot') || (lowerMsg.includes('ranking') && (lowerMsg.includes('plot') || lowerMsg.includes('chart'))) || lowerMsg.includes('vulnerability plot')) {
            vizInfo.type = 'vulnerability_plot';
        }
        // Then map types
        else if (lowerMsg.includes('variable') && lowerMsg.includes('map')) {
            vizInfo.type = 'variable_map';
            vizInfo.variable = extractVariable(message);
        } else if (lowerMsg.includes('normalized') && lowerMsg.includes('map')) {
            vizInfo.type = 'normalized_map';
            vizInfo.variable = extractVariable(message);
        } else if (lowerMsg.includes('composite map') || (lowerMsg.includes('risk') && lowerMsg.includes('map') && !lowerMsg.includes('vulnerability'))) {
            vizInfo.type = 'composite_map';
        } else if (lowerMsg.includes('vulnerability') && lowerMsg.includes('map')) {
            vizInfo.type = 'vulnerability_map';
        } else if (lowerMsg.includes('urban extent') || (lowerMsg.includes('urban') && lowerMsg.includes('map'))) {
            vizInfo.type = 'urban_extent_map';
            vizInfo.threshold = extractThreshold(message); // Defaults to 30 if not found
        }
         // Fallback if just "map" or "plot" is mentioned with a variable
         else if (vizInfo.type === null && (lowerMsg.includes('map') || lowerMsg.includes('plot') || lowerMsg.includes('chart'))) {
            const extractedVar = extractVariable(message);
            if (extractedVar) {
                vizInfo.type = 'variable_map'; // Default to variable map if a variable is found
               vizInfo.variable = extractedVar;
           } else if (lowerMsg.includes('map')) {
                vizInfo.type = 'composite_map'; // Default map if no variable found
           } else if (lowerMsg.includes('plot')) {
               vizInfo.type = 'vulnerability_plot'; // Default plot if no variable found
           }
       }


       console.log(`Extracted viz info: type=${vizInfo.type}, variable=${vizInfo.variable}, threshold=${vizInfo.threshold}`);
       return vizInfo;
   }


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

       // If no pattern matched, check for known variable names directly in the message
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


   function extractThreshold(message) {
       const thresholdMatch = message.match(/(\d+)\s*%/); // Looks for "number%"
       if (thresholdMatch && thresholdMatch[1]) {
           return parseInt(thresholdMatch[1]);
       }
       // Look for "threshold of number" or "at number"
       const numberMatch = message.match(/(?:threshold\s+(?:of\s+)?|at\s+)(\d+)/i);
       if (numberMatch && numberMatch[1]) {
           return parseInt(numberMatch[1]);
       }
       return 30; // Default threshold
   }


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


   // Updated function for Phase 3 - Shows a message prompting user to confirm via text chat rather than buttons
   function showCustomAnalysisConfirmation(variables) {
       // Remove previous confirmation messages if any
       const existingConfirmations = chatMessages.querySelectorAll('.custom-analysis-confirmation');
       existingConfirmations.forEach(el => el.closest('.message').remove());


       let confirmationMessage = `<strong>Confirm Custom Analysis:</strong><br>`;
       let detailsMessage = '';

       if (variables && variables.length > 0) {
           detailsMessage += `I can run the analysis using these variables:<br><ul style="margin-top: 5px;">${variables.map(v => `<li>${v}</li>`).join('')}</ul>`;
           // Phase 3: No more buttons - instruct user to respond in chat
           detailsMessage += `<p>Please respond with "yes", "confirm", or "no" to proceed or cancel.</p>`;
       } else {
           detailsMessage = "I couldn't clearly identify the variables you want to use. Could you please list them more explicitly, like 'run analysis with variables: rainfall, temperature, population'? Or ask to run the standard analysis.<br>"
       }

       addSystemMessage(`
           <div class="custom-analysis-confirmation">
               ${confirmationMessage}${detailsMessage}
           </div>
       `);
       // No need to hide typing/set waiting here, user needs to interact
   }


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


   function uploadCSV(file) {
       const formData = new FormData(); formData.append('file', file);
       csvUploadStatus.innerHTML = '<div class="spinner-border spinner-border-sm text-primary" role="status"></div> Uploading...';
       csvUploadStatus.className = "upload-status";
       showLoadingIndicator();
       fetch('/upload_csv', { method: 'POST', body: formData })
       .then(response => response.json())
       .then(data => {
           hideLoadingIndicator();
           if (data.status === 'success') {
               csvUploadStatus.textContent = `Success: ${file.name} uploaded.`;
               csvUploadStatus.className = "upload-status success";
               sessionData.csvLoaded = true; updateSessionStatus();
               addSystemMessage(`<strong>CSV data loaded:</strong> ${file.name} (Rows: ${data.rows}, Columns: ${data.columns})`);
               if (data.analysis_prompt) addAssistantMessage(data.analysis_prompt);
               else if (sessionData.shapefileLoaded) addAssistantMessage("Both files loaded. You can now 'Run the analysis'.");
               else addAssistantMessage("CSV file loaded. Please upload the Shapefile (ZIP) next.");
               if (uploadModal) uploadModal.hide(); // Close modal on success
           } else {
               csvUploadStatus.textContent = `Error: ${data.message}`;
               csvUploadStatus.className = "upload-status error";
               addSystemMessage(`<strong>CSV Upload Error:</strong> ${data.message}`);
           }
            scrollToBottom();
       }).catch(error => {
           hideLoadingIndicator();
           console.error('Error uploading CSV:', error);
           csvUploadStatus.textContent = "Upload failed. Please try again.";
           csvUploadStatus.className = "upload-status error";
           addSystemMessage("<strong>CSV Upload Error:</strong> Network or server issue.");
       });
   }

   function uploadShapefile(file) {
       const formData = new FormData(); formData.append('file', file);
       shapefileUploadStatus.innerHTML = '<div class="spinner-border spinner-border-sm text-primary" role="status"></div> Uploading...';
       shapefileUploadStatus.className = "upload-status";
        showLoadingIndicator();
       fetch('/upload_shapefile', { method: 'POST', body: formData })
       .then(response => response.json())
       .then(data => {
            hideLoadingIndicator();
           if (data.status === 'success' || data.status === 'warning') { // Handle success and warning
               shapefileUploadStatus.textContent = data.status === 'warning' ? `Warning: ${data.message}` : `Success: ${file.name} uploaded.`;
               shapefileUploadStatus.className = `upload-status ${data.status}`; // 'success' or 'warning'
               sessionData.shapefileLoaded = true; updateSessionStatus();
                addSystemMessage(`<strong>Shapefile loaded:</strong> ${file.name} (Features: ${data.features})${data.status === 'warning' ? '. '+data.message : ''}`);
               if (data.mismatches) { // Display mismatches if any
                   addSystemMessage(`Potential WardName mismatches found: ${JSON.stringify(data.mismatches.slice(0,5))}${data.mismatches.length > 5 ? '...' : ''}`);
               }
               if (data.analysis_prompt) addAssistantMessage(data.analysis_prompt);
                else if (sessionData.csvLoaded) addAssistantMessage("Both files loaded. You can now 'Run the analysis'.");
               else addAssistantMessage("Shapefile loaded. Please upload the CSV/Excel file next.");
                if (uploadModal) uploadModal.hide(); // Close modal on success/warning
           } else {
               shapefileUploadStatus.textContent = `Error: ${data.message}`;
               shapefileUploadStatus.className = "upload-status error";
                addSystemMessage(`<strong>Shapefile Upload Error:</strong> ${data.message}`);
           }
            scrollToBottom();
       }).catch(error => {
           hideLoadingIndicator();
           console.error('Error uploading shapefile:', error);
           shapefileUploadStatus.textContent = "Upload failed. Please try again.";
           shapefileUploadStatus.className = "upload-status error";
           addSystemMessage("<strong>Shapefile Upload Error:</strong> Network or server issue.");
       });
   }


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
        // Add an error state if needed, e.g. sessionData.error = true;
        // else if (sessionData.error) {
        //     currentStatusText = "Error Occurred";
        //     currentDotClass = "status-dot error"; // Red
        // }

        if (statusTextElement) {
            statusTextElement.textContent = currentStatusText;
        }
        if (statusDot) {
            statusDot.className = currentDotClass;
        }
        // If you want to change the class of the overall indicator div:
        // if (sessionStatusIndicator) {
        //    sessionStatusIndicator.className = `session-status-indicator ${currentDotClass.replace('status-dot ', '')}-indicator-bg`; // Example for bg
        // }
    }


   function getLanguageName(code) {
       const languages = {'en':'English','ha':'Hausa','yo':'Yoruba','ig':'Igbo','ff':'Fulfulde','kr':'Kanuri','fr':'French','ar':'Arabic'};
       return languages[code] || 'English'; // Default to English
   }


   function getVisualizationTitle(vizType, variableName = null) {
       let title = 'Visualization'; // Default
       const fullVarName = variableName ? getFullVariableNameToDisplay(variableName) : 'Selected Variable';

       switch (vizType) {
           case 'variable_map': title = `Variable: ${fullVarName}`; break;
           case 'normalized_map': title = `Normalized: ${fullVarName}`; break;
           case 'composite_map': title = 'Composite Risk Maps'; break;
           case 'vulnerability_map': title = 'Ward Vulnerability Map'; break;
           case 'vulnerability_plot': title = 'Ward Vulnerability Ranking'; break;
           case 'urban_extent_map': title = 'Urban Extent Analysis'; break;
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

}); // End DOMContentLoaded