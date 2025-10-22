// Other Field Validation
// Prevents numerical-only entries in "other" text fields for dropdown questions

const OtherFieldValidation = {
    validatedFields: new Set(),
    validationErrors: new Map(),
    
    // Initialize validation for fields with showOtherItem
    init: function() {
        if (typeof survey === 'undefined' || survey === null) {
            console.log("Survey not initialized yet, skipping other field validation setup");
            return;
        }
        
        console.log("Setting up other field validation");
        
        // Find all questions with showOtherItem enabled
        const questionsWithOther = [];
        survey.getAllQuestions().forEach(question => {
            if (question.showOtherItem === true) {
                questionsWithOther.push(question);
                this.validatedFields.add(question.name);
            }
        });
        
        console.log(`Found ${questionsWithOther.length} questions with showOtherItem:`, questionsWithOther.map(q => q.name));
        
        // Set up the onValidateQuestion event handler
        this.setupValidationEventHandler();
        
        // Configure survey to validate on value changes for immediate feedback
        survey.checkErrorsMode = "onValueChanged";
        
        // Set up DOM observers to catch dynamically created other inputs
        this.setupDOMObserver();
    },
    
    // Set up the onValidateQuestion event handler
    setupValidationEventHandler: function() {
        const self = this;
        
        // Add the validation event handler to the survey
        survey.onValidateQuestion.add(function(sender, options) {
            console.log(`Validating question: ${options.name}, value:`, options.value);
            
            // Check if this is a question with showOtherItem that we need to validate
            if (!self.validatedFields.has(options.name)) {
                return; // Not a question we're validating
            }
            
            const question = survey.getQuestionByName(options.name);
            if (!question || !question.showOtherItem) {
                return; // Not an "other" question
            }
            
            // Check if "other" is selected
            const isOtherSelected = options.value === 'other' || 
                                   options.value === survey.otherValue ||
                                   (Array.isArray(options.value) && options.value.includes('other')) ||
                                   question.isOtherSelected;
            
            if (!isOtherSelected) {
                // Clear any validation errors for this field
                self.validationErrors.delete(options.name);
                return;
            }
            
            // Get the comment value
            const commentValue = question.comment;
            console.log(`Checking other comment for ${options.name}:`, commentValue);
            
            if (!commentValue || !commentValue.trim()) {
                // No comment or empty comment - let required validation handle this
                self.validationErrors.delete(options.name);
                return;
            }
            
            // Check if the comment is numeric-only
            const isNumericOnly = /^\s*\d+\s*$/.test(commentValue.trim());
            
            if (isNumericOnly) {
                console.log(`Setting validation error for ${options.name} - numeric comment detected`);
                self.validationErrors.set(options.name, true);
                options.error = "🛑 Please enter text, not just numbers";
            } else {
                console.log(`Validation passed for ${options.name}`);
                self.validationErrors.delete(options.name);
            }
        });
    },
    
    // Main validation logic for other fields
    validateOtherField: function(fieldName, value, question) {
        // Check if "other" is selected (either by value or if question has otherValue set)
        const hasOtherSelected = value === 'other' || 
                                value === survey.otherValue || 
                                (Array.isArray(value) && value.includes('other')) ||
                                question.isOtherSelected;
        
        if (!hasOtherSelected) {
            // Clear any existing validation error for this field
            this.validationErrors.delete(fieldName);
            return true; // Not an "other" selection, so validation passes
        }
        
        // Get the other text input value
        const otherText = this.getOtherTextValue(question);
        
        if (!otherText || otherText.trim() === '') {
            // Clear validation error for empty text (handled by required validation)
            this.validationErrors.delete(fieldName);
            return true; // Empty other text is handled by SurveyJS required validation
        }
        
        // Check if the other text is numerical only
        const isNumericOnly = /^\s*\d+\s*$/.test(otherText.trim());
        
        if (isNumericOnly) {
            // Track this validation error
            this.validationErrors.set(fieldName, true);
            return {
                isValid: false,
                errorText: "Please enter text, not just numbers"
            };
        }
        
        // Clear validation error if validation passes
        this.validationErrors.delete(fieldName);
        return true; // Validation passes
    },
    
    // Validation method specifically for form submission
    validateOtherFieldForSubmission: function(fieldName, question) {
        // Check if "other" is selected
        const hasOtherSelected = question.value === 'other' || 
                                question.value === survey.otherValue || 
                                (Array.isArray(question.value) && question.value.includes('other')) ||
                                question.isOtherSelected;
        
        if (!hasOtherSelected) {
            this.validationErrors.delete(fieldName);
            return true;
        }
        
        // Get the other text input value
        const otherText = this.getOtherTextValue(question);
        
        if (!otherText || otherText.trim() === '') {
            this.validationErrors.delete(fieldName);
            return true;
        }
        
        // Check if the other text is numerical only
        const isNumericOnly = /^\s*\d+\s*$/.test(otherText.trim());
        
        if (isNumericOnly) {
            this.validationErrors.set(fieldName, true);
            return {
                isValid: false,
                errorText: "Please enter text, not just numbers"
            };
        }
        
        this.validationErrors.delete(fieldName);
        return true;
    },

    // Check if there are any validation errors
    hasErrors: function() {
        return this.validationErrors.size > 0;
    },
    
    // Focus the first field with a validation error
    focusFirstError: function() {
        for (const [fieldName] of this.validationErrors) {
            const question = survey.getQuestionByName(fieldName);
            if (question) {
                const questionEl = document.querySelector(`[data-name="${fieldName}"]`);
                if (questionEl) {
                    const offset = 100;
                    const rect = questionEl.getBoundingClientRect();
                    const absoluteTop = rect.top + window.pageYOffset - offset;
                    
                    window.scrollTo({
                        top: absoluteTop,
                        behavior: 'smooth'
                    });
                    
                    // Focus the other input field if possible
                    const otherInput = questionEl.querySelector('input[id*="other"], textarea[id*="other"]');
                    if (otherInput) {
                        setTimeout(() => {
                            otherInput.focus();
                        }, 500);
                    }
                    break;
                }
            }
        }
    },
    
    // Get the current value of the "other" text input for a question
    getOtherTextValue: function(question) {
        // Try multiple approaches to get the other text value
        
        // Method 1: From question comment (most common)
        if (question.comment) {
            return question.comment;
        }
        
        // Method 2: From survey's other value tracking
        const otherValue = survey.getOtherValue && survey.getOtherValue(question.name);
        if (otherValue) {
            return otherValue;
        }
        
        // Method 3: Direct DOM lookup for the other input
        const questionElement = document.querySelector(`[data-name="${question.name}"]`);
        if (questionElement) {
            const otherInput = questionElement.querySelector('input[id*="other"], input[name*="other"], textarea[id*="other"]');
            if (otherInput) {
                return otherInput.value;
            }
        }
        
        return null;
    },
    
    // Set up DOM observer to catch dynamically created other inputs
    setupDOMObserver: function() {
        if (this.observer) {
            this.observer.disconnect(); // Clean up existing observer
        }
        
        const surveyContainer = document.getElementById('surveyContainer');
        if (!surveyContainer) {
            console.log("Survey container not found, deferring DOM observer setup");
            return;
        }
        
        this.observer = new MutationObserver((mutations) => {
            mutations.forEach((mutation) => {
                mutation.addedNodes.forEach((node) => {
                    if (node.nodeType === Node.ELEMENT_NODE) {
                        this.setupInputValidation(node);
                    }
                });
            });
        });
        
        this.observer.observe(surveyContainer, {
            childList: true,
            subtree: true
        });
        
        // Also check existing inputs
        this.setupInputValidation(surveyContainer);
    },
    
    // Set up real-time validation on other input elements
    setupInputValidation: function(container) {
        // Find all "other" input elements
        const otherInputs = container.querySelectorAll('input[id*="other"], input[name*="other"], textarea[id*="other"], textarea[name*="other"]');
        
        otherInputs.forEach(input => {
            if (input.dataset.otherValidationSetup === 'true') {
                return; // Already set up
            }
            
            input.dataset.otherValidationSetup = 'true';
            console.log('Setting up real-time validation for other input:', input);
            
            // Add real-time validation
            const self = this;
            const validateInput = function() {
                const value = input.value.trim();
                const isNumericOnly = /^\s*\d+\s*$/.test(value);
                
                // Find the associated question to track errors properly
                const questionElement = input.closest('[data-name]');
                const fieldName = questionElement ? questionElement.getAttribute('data-name') : null;
                
                // Remove existing error styling
                input.classList.remove('sv-other-field-error');
                const existingError = input.parentElement.querySelector('.sv-other-field-error-message');
                if (existingError) {
                    existingError.remove();
                }
                
                if (value && isNumericOnly) {
                    // Track validation error
                    if (fieldName) {
                        self.validationErrors.set(fieldName, true);
                    }
                    
                    // Add error styling
                    input.classList.add('sv-other-field-error');
                } else {
                    // Clear validation error
                    if (fieldName) {
                        self.validationErrors.delete(fieldName);
                    }
                    input.classList.remove('sv-other-field-error');
                }
            };
            
            // Add event listeners for real-time validation
            input.addEventListener('input', validateInput);
            input.addEventListener('blur', validateInput);
            
            // Initial validation
            if (input.value) {
                validateInput();
            }
        });
    },
    
    // Clean up
    destroy: function() {
        if (this.observer) {
            this.observer.disconnect();
            this.observer = null;
        }
        this.validatedFields.clear();
    }
};

// CSS styles for error states
const otherFieldValidationCSS = `
    .sv-other-field-error {
        border-color: #ed4956 !important;
        box-shadow: 0 0 0 1px #ed4956 !important;
    }
    
    .sv-other-field-error-message {
        color: #ed4956;
        font-size: 12px;
        margin-top: 4px;
        line-height: 1.4;
    }
`;

// Inject CSS
if (!document.getElementById('other-field-validation-styles')) {
    const styleSheet = document.createElement('style');
    styleSheet.id = 'other-field-validation-styles';
    styleSheet.textContent = otherFieldValidationCSS;
    document.head.appendChild(styleSheet);
}