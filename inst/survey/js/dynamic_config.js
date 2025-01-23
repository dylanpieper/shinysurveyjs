// Normalize field value for comparison
function normalizeFieldValue(value, settings = {}) {
    if (!value || typeof value !== 'string') return value;

    const {
        removeSpecial = true
    } = settings;

    let normalized = value.toLowerCase().trim().replace(/\s+/g, ' ');

    if (removeSpecial) {
        normalized = normalized.replace(/[^a-z0-9\s]/g, '');
    }

    return normalized;
}

const UniqueValidation = {
    fields: new Map(),
    errorElements: new Map(),

    initField(fieldName, config) {
        this.fields.set(fieldName, config);
        this.setupErrorDisplay(fieldName);
    },

    setupErrorDisplay(fieldName) {
        const errorDiv = document.createElement('div');
        errorDiv.className = 'unique-validation-error';
        errorDiv.style.cssText = `
            color: #d92929;
            font-size: 14px;
            padding: 8px 0;
            display: none;
            margin-top: 4px;
            font-family: inherit;
            font-weight: bold;
        `;
        this.errorElements.set(fieldName, errorDiv);

        // Watch for the question element and attach error display
        const observer = new MutationObserver(() => {
            const questionEl = document.querySelector(`[data-name="${fieldName}"]`);
            if (questionEl && !questionEl.querySelector('.unique-validation-error')) {
                questionEl.appendChild(errorDiv);
                observer.disconnect();

                // Set up input monitoring
                const inputEl = questionEl.querySelector('input') ||
                              questionEl.querySelector('textarea');

                if (inputEl) {
                    let lastValue = inputEl.value;

                    // Monitor all possible input methods
                    ['input', 'change', 'blur', 'keyup'].forEach(eventType => {
                        inputEl.addEventListener(eventType, () => {
                            const currentValue = inputEl.value;
                            if (currentValue !== lastValue) {
                                lastValue = currentValue;
                                this.validateField(fieldName, currentValue);
                            }
                        });
                    });
                }
            }
        });

        observer.observe(document.body, {
            childList: true,
            subtree: true
        });
    },

    validateField(fieldName, value) {
        console.log(`Validating ${fieldName}:`, value);

        const config = this.fields.get(fieldName);
        const errorEl = this.errorElements.get(fieldName);

        if (!config || !errorEl) return true;

        // Always hide error initially
        errorEl.style.display = 'none';

        if (!value) return true;

        const normalizedInput = this.normalizeValue(value, config.normalization_settings);
        const matches = config.normalized_values.some(item =>
            item.normalized === normalizedInput
        );

        if (matches && config.result === "stop") {
            // Show error
            errorEl.textContent = "This value already exists. Please enter a unique value.";
            errorEl.style.display = 'block';
            return false;
        } else if (matches && config.result === "warn") {
            // Handle warning field
            const warningField = survey.getQuestionByName(config.result_field);
            if (warningField) {
                warningField.visible = true;
            }
        } else if (config.result === "warn") {
            // Hide warning field
            const warningField = survey.getQuestionByName(config.result_field);
            if (warningField) {
                warningField.visible = false;
            }
        }

        return true;
    },

    normalizeValue(value, settings = {}) {
        if (!value || typeof value !== 'string') return value;
        let normalized = value.toLowerCase().trim().replace(/\s+/g, ' ');
        if (settings.removeSpecial) {
            normalized = normalized.replace(/[^a-z0-9\s]/g, '');
        }
        return normalized;
    },

    hasErrors() {
        // Check all error elements for visibility
        for (const errorEl of this.errorElements.values()) {
            if (errorEl.style.display === 'block') return true;
        }
        return false;
    },

    getFirstErrorField() {
        for (const [fieldName, errorEl] of this.errorElements) {
            if (errorEl.style.display === 'block') return fieldName;
        }
        return null;
    },

    focusField(fieldName) {
        const questionEl = document.querySelector(`[data-name="${fieldName}"]`);
        if (!questionEl) return;

        const inputEl = questionEl.querySelector('input') ||
                       questionEl.querySelector('textarea') ||
                       questionEl;

        // Scroll into view
        inputEl.scrollIntoView({ behavior: 'smooth', block: 'center' });

        // Focus after scroll
        setTimeout(() => {
            try {
                inputEl.focus();
            } catch (e) {
                console.warn('Could not focus field:', e);
            }
        }, 500);
    }
};

function setupUniqueValidation(data) {
    if (!data.unique_validation) return;

    const attemptSetup = () => {
        if (typeof survey === 'undefined' || survey === null) {
            console.error("Survey not initialized");
            return;
        }

        // Initialize validation for each field
        Object.entries(data.unique_validation).forEach(([fieldName, config]) => {
            UniqueValidation.initField(fieldName, config);

            // Add survey-level validation as backup
            survey.onValueChanged.add((sender, options) => {
                if (options.name === fieldName && options.value !== undefined) {
                    UniqueValidation.validateField(fieldName, options.value);
                }
            });
        });

        // Handle page navigation
        survey.onCurrentPageChanging.add((sender, options) => {
            if (UniqueValidation.hasErrors()) {
                const errorField = UniqueValidation.getFirstErrorField();
                if (errorField) {
                    UniqueValidation.focusField(errorField);
                    options.allow = false;
                }
            }
        });

        // Handle form completion
        survey.onCompleting.add((sender, options) => {
            if (UniqueValidation.hasErrors()) {
                const errorField = UniqueValidation.getFirstErrorField();
                if (errorField) {
                    UniqueValidation.focusField(errorField);
                    options.allow = false;
                }
            }
        });
    };

    attemptSetup();
}

// Helper functions for dynamic configuration
function setChoices(question, choicesData) {
    if (!choicesData || !Array.isArray(choicesData.value)) {
        console.warn(`Invalid choices data for ${question.name}`);
        return;
    }

    const surveyChoices = choicesData.value.map((value, index) => ({
        value: value,
        text: choicesData.text[index]
    }));

    question.choices = surveyChoices;
}

function processChildField(fieldName, fieldData) {
    survey.childConfig = survey.childConfig || {};
    survey.childConfig[fieldName] = fieldData;
}

function setupParentHandler(fieldName, fieldData) {
    survey.parentConfig = survey.parentConfig || {};
    survey.parentConfig[fieldName] = {
        data: data,
        childField: fieldData.childField,
        fieldData: fieldData,
        type: fieldData.type,
        valueToIdMap: fieldData.choices.value.reduce((map, value, index) => {
            map[value] = fieldData.choices.ids[index];
            return map;
        }, {}),
        choicesList: fieldData.choices
    };

    if (survey.onValueChanged) {
        const existingHandlers = survey.onValueChanged.actions || [];
        survey.onValueChanged.actions = existingHandlers.filter(
            handler => handler.name !== `update_${fieldName}_children`
        );
    }

    const handlerFunction = function(sender, options) {
        if (options.name !== fieldName) return;
        updateChildFields(fieldName, options.value);
        saveSurveyProgress(survey);
    };

    survey.onValueChanged.add(handlerFunction, `update_${fieldName}_children`);

    const currentValue = survey.getQuestionByName(fieldName).value;
    if (currentValue) {
        updateChildFields(fieldName, currentValue);
    }
}

function updateChildFields(parentField, parentValue) {
    if (!parentValue) return;

    const parentConfig = survey.parentConfig[parentField];
    if (!parentConfig) {
        console.warn(`No parent config found for ${parentField}`);
        return;
    }

    const childField = parentConfig.childField;
    if (!childField) {
        console.warn(`No child field found for parent ${parentField}`);
        return;
    }

    const childData = survey.childConfig[childField];
    if (!childData) {
        console.warn(`No child data found for ${childField}`);
        return;
    }

    const childQuestion = survey.getQuestionByName(childField);
    if (!childQuestion) {
        console.warn(`Child question ${childField} not found`);
        return;
    }

    const parentId = parentConfig.valueToIdMap[parentValue];
    const relevantChoices = [];
    const choicesData = childData.choices;

    choicesData.value.forEach((value, idx) => {
        if (choicesData.parentId[idx] === parentId) {
            relevantChoices.push({
                value: value,
                text: choicesData.text[idx]
            });
        }
    });

    childQuestion.choices = relevantChoices;

    if (childQuestion.value && !relevantChoices.some(c => c.value === childQuestion.value)) {
        childQuestion.value = null;
    }

    saveSurveyProgress(survey);
}

function updateChildFieldsFromParents() {
    if (!survey.parentConfig) {
        console.warn("No parent config found");
        return;
    }

    Object.entries(survey.parentConfig).forEach(([parentField, config]) => {
        const parentQuestion = survey.getQuestionByName(parentField);
        if (parentQuestion && parentQuestion.value) {
            updateChildFields(parentField, parentQuestion.value);
        }
    });
}

// Update dynamic choices based on incoming data
function updateDynamicChoices(data) {
    const metrics = {
        processedFields: {
            standalone: 0,
            child: 0,
            paramParent: 0,
            choiceParent: 0
        },
        updatedChoices: 0,
        errors: [],
        warnings: []
    };

    const attemptUpdate = (retries = 3) => {
        if (typeof survey === 'undefined' || survey === null) {
            if (retries > 0) {
                setTimeout(() => attemptUpdate(retries - 1), CHOICE_UPDATE_DELAY);
                return;
            }
            document.getElementById("surveyNotDefinedMessage").style.display = "block";
            metrics.errors.push("Survey not initialized after retries");
            console.error("Summary:", metrics);
            return;
        }

        survey.dynamicConfig = data;

        // Process standalone and child fields
        Object.entries(data).forEach(([fieldName, fieldData]) => {
            if (fieldData.type === "standalone" || fieldData.type === "child") {
                const targetQuestion = survey.getQuestionByName(fieldName);
                if (!targetQuestion) {
                    metrics.warnings.push(`Target question not found: ${fieldName}`);
                    return;
                }

                if (fieldData.type === "standalone") {
                    setChoices(targetQuestion, fieldData.choices);
                    metrics.processedFields.standalone++;
                } else {
                    processChildField(fieldName, fieldData);
                    metrics.processedFields.child++;
                }
            }
        });

        // Process parent fields
        Object.entries(data).forEach(([fieldName, fieldData]) => {
            if (fieldData.type === "param_parent" || fieldData.type === "choice_parent") {
                const targetQuestion = survey.getQuestionByName(fieldName);
                if (!targetQuestion) {
                    metrics.warnings.push(`Target question not found: ${fieldName}`);
                    return;
                }

                setupParentHandler(fieldName, fieldData);

                if (fieldData.type === "choice_parent") {
                    setChoices(targetQuestion, fieldData.choices);
                    metrics.processedFields.choiceParent++;
                } else {
                    metrics.processedFields.paramParent++;
                }
            }
        });

        // Update child fields and set up unique validation
        setTimeout(() => {
            updateChildFieldsFromParents();

            if (data.unique_validation) {
                setupUniqueValidation(data);
            }

            // Signal that dynamic configuration is complete
            console.log("Dynamic configuration complete");
            Shiny.setInputValue("dynamicConfigComplete", true);

            // Now show the survey container and hide the loading message
            document.getElementById("surveyContainer").style.display = "block";
            document.getElementById("waitingMessage").style.display = "none";

        }, CHOICE_UPDATE_DELAY);

        // Log final summary
        console.log("Dynamic choices update summary:", {
            ...metrics,
            totalProcessedFields: Object.values(metrics.processedFields).reduce((a, b) => a + b, 0),
            totalWarnings: metrics.warnings.length,
            totalErrors: metrics.errors.length
        });
    };

    // Start the update process
    attemptUpdate();
}

// Handler registration
Shiny.addCustomMessageHandler("updateDynamicChoices", updateDynamicChoices);

// Cookie handling functions remain the same
function saveSurveyProgress(survey) {
    try {
        if (!survey || !survey.data) {
            return;
        }

        const dataToStore = { ...survey.data };

        if (survey.dynamicConfig) {
            dataToStore._dynamicConfig = {
                parentFields: {},
                childChoices: {}
            };

            Object.entries(survey.parentConfig || {}).forEach(([parentField, config]) => {
                const parentQuestion = survey.getQuestionByName(parentField);
                if (parentQuestion) {
                    dataToStore._dynamicConfig.parentFields[parentField] = {
                        value: parentQuestion.value,
                        type: config.type,
                        choices: config.choicesList
                    };

                    const childField = config.childField;
                    if (childField) {
                        const childQuestion = survey.getQuestionByName(childField);
                        if (childQuestion) {
                            dataToStore._dynamicConfig.childChoices[childField] = {
                                choices: childQuestion.choices,
                                value: childQuestion.value,
                                parentField: parentField
                            };
                        }
                    }
                }
            });
        }

        setCookie(COOKIE_NAME, dataToStore, COOKIE_EXPIRATION_DAYS);
    } catch (error) {
        console.error("Error saving survey progress:", error);
    }
}

function restoreDynamicChoices(survey, savedData) {
    try {
        if (!savedData || !savedData._dynamicConfig) {
            return;
        }

        const { parentFields, childChoices } = savedData._dynamicConfig;

        Object.entries(parentFields).forEach(([parentField, config]) => {
            const parentQuestion = survey.getQuestionByName(parentField);
            if (parentQuestion) {
                if (config.type === "choice_parent" && config.choices) {
                    parentQuestion.choices = config.choices;
                }

                if (config.value !== undefined) {
                    parentQuestion.value = config.value;
                }
            }
        });

        Object.entries(childChoices).forEach(([childField, config]) => {
            const childQuestion = survey.getQuestionByName(childField);
            if (childQuestion && config.choices) {
                childQuestion.choices = config.choices;

                if (config.value !== undefined &&
                    config.choices.some(choice => choice.value === config.value)) {
                    childQuestion.value = config.value;
                }
            }
        });
    } catch (error) {
        console.error("Error restoring dynamic choices:", error);
    }
}

function restoreDynamicChoices(survey, savedData) {
    if (!savedData || !savedData._dynamicConfig) return;

    console.log("Restoring dynamic choices:", savedData._dynamicConfig);
    const { parentFields, childChoices } = savedData._dynamicConfig;

    // First restore parent values
    Object.entries(parentFields).forEach(([parentField, config]) => {
        const parentQuestion = survey.getQuestionByName(parentField);
        if (parentQuestion) {
            console.log(`Restoring parent ${parentField} value:`, config.value);
            parentQuestion.value = config.value;
        }
    });

    // Then restore child choices and values
    Object.entries(childChoices).forEach(([childField, config]) => {
        const childQuestion = survey.getQuestionByName(childField);
        if (childQuestion) {
            console.log(`Restoring child ${childField} choices:`, config.choices);
            childQuestion.choices = config.choices;
            if (config.value !== undefined &&
                config.choices.some(choice => choice.value === config.value)) {
                childQuestion.value = config.value;
            }
        }
    });
}
