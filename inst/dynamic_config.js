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
    validationErrors: new Map(),

    initField(fieldName, config) {
        this.fields.set(fieldName, config);

        const targetQuestion = survey.getQuestionByName(fieldName);
        if (targetQuestion) {
            const validateAndShowError = (value) => {
                const resultField = survey.getQuestionByName(config.result_field);

                if (!value) {
                    this.validationErrors.delete(fieldName);
                    if (resultField) {
                        resultField.visible = false;
                    }
                    return true;
                }

                const normalizedInput = this.normalizeValue(value, config.normalization_settings);
                const matches = config.normalized_values.some(item =>
                    item.normalized === normalizedInput
                );

                if (resultField) {
                    resultField.visible = matches;
                }

                if (matches && config.result === "stop") {
                    this.validationErrors.set(fieldName, true);
                    return false;
                } else {
                    this.validationErrors.delete(fieldName);
                    return true;
                }
            };

            // Add immediate validation during typing
            survey.onValueChanging.add((sender, options) => {
                if (options.name === fieldName) {
                    validateAndShowError(options.value);
                }
            });

            // Keep valueChangedCallback for final validation
            targetQuestion.valueChangedCallback = function() {
                validateAndShowError(this.value);
            };

            // Add validator for Survey.js validation system
            targetQuestion.validators = targetQuestion.validators || [];
            targetQuestion.validators.push({
                type: "custom",
                validate: (value, options) => {
                    return validateAndShowError(value);
                }
            });
        }
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
        return this.validationErrors.size > 0;
    },

    focusFirstError() {
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

                    setTimeout(() => {
                        const input = questionEl.querySelector('input, textarea');
                        if (input) {
                            input.focus();
                        }
                    }, 500);
                }
                return true;
            }
        }
        return false;
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
            if (!config.result_field) {
                console.error(`Missing result_field for ${fieldName}`);
                return;
            }
            UniqueValidation.initField(fieldName, config);
        });

        // Block page navigation if there are validation errors
        survey.onCurrentPageChanging.add((sender, options) => {
            if (UniqueValidation.hasErrors()) {
                options.allow = false;
                UniqueValidation.focusFirstError();
            }
        });

        // Block survey completion if there are validation errors
        survey.onCompleting.add((sender, options) => {
            if (UniqueValidation.hasErrors()) {
                options.allow = false;
                UniqueValidation.focusFirstError();
            }
        });

        // Revalidate current page when returning to it
        survey.onCurrentPageChanged.add((sender, options) => {
            const questions = sender.currentPage.questions;
            questions.forEach(question => {
                if (UniqueValidation.validationErrors.has(question.name)) {
                    question.hasErrors(true);
                }
            });
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
