function updateDynamicChoices(data) {
    function updateChildFieldsFromParents() {
        console.log("Running updateChildFieldsFromParents");
        if (!survey.parentConfig) {
            console.log("No parent config found");
            return;
        }

        Object.entries(survey.parentConfig).forEach(([parentField, config]) => {
            const parentQuestion = survey.getQuestionByName(parentField);
            if (parentQuestion && parentQuestion.value) {
                console.log(`Updating children of ${parentField} with value ${parentQuestion.value}`);
                updateChildFields(parentField, parentQuestion.value);
            }
        });
    }

    function processChildField(fieldName, fieldData) {
        console.log(`Processing child field ${fieldName}:`, fieldData);
        survey.childConfig = survey.childConfig || {};
        survey.childConfig[fieldName] = fieldData;
    }

    function setChoices(question, choicesData) {
        console.log(`Setting choices for ${question.name}:`, choicesData);
        if (!choicesData || !Array.isArray(choicesData.value)) {
            console.warn(`Invalid choices data for ${question.name}`);
            return;
        }

        const surveyChoices = choicesData.value.map((value, index) => ({
            value: value,
            text: choicesData.text[index]
        }));

        console.log(`Final choices for ${question.name}:`, surveyChoices);
        question.choices = surveyChoices;
    }

    function setupParentHandler(fieldName, fieldData) {
        console.log(`Setting up parent handler for ${fieldName}:`, fieldData);

        // Store configuration
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

        // Remove existing handler if any
        if (survey.onValueChanged) {
            const existingHandlers = survey.onValueChanged.actions || [];
            survey.onValueChanged.actions = existingHandlers.filter(
                handler => handler.name !== `update_${fieldName}_children`
            );
        }

        // Add value change handler
        const handlerFunction = function(sender, options) {
            if (options.name !== fieldName) return;
            console.log(`Value changed: ${fieldName} ${options.value}`);
            updateChildFields(fieldName, options.value);
            saveSurveyProgress(survey);
        };

        survey.onValueChanged.add(handlerFunction, `update_${fieldName}_children`);

        // Handle initial value if exists
        const currentValue = survey.getQuestionByName(fieldName).value;
        if (currentValue) {
            console.log(`Updating children based on current value ${currentValue}`);
            updateChildFields(fieldName, currentValue);
        }
    }

    function updateChildFields(parentField, parentValue) {
        console.log(`Updating child fields for parent ${parentField} = ${parentValue}`);

        if (!parentValue) return;

        const parentConfig = survey.parentConfig[parentField];
        if (!parentConfig) {
            console.warn(`No parent config found for ${parentField}`);
            return;
        }

        const childField = parentConfig.childField;
        console.log(`Found child field: ${childField}`);
        if (!childField) {
            console.warn(`No child field found for parent ${parentField}`);
            return;
        }

        const childData = survey.childConfig[childField];
        console.log(`Found child data:`, childData);
        if (!childData) {
            console.warn(`No child data found for ${childField}`);
            return;
        }

        const childQuestion = survey.getQuestionByName(childField);
        if (!childQuestion) {
            console.warn(`Child question ${childField} not found`);
            return;
        }

        // Get parent ID from the value
        const parentId = parentConfig.valueToIdMap[parentValue];
        console.log(`Mapped parent value ${parentValue} to ID ${parentId}`);

        // Filter choices based on parent ID
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

        console.log(`Setting ${relevantChoices.length} choices for ${childField}:`, relevantChoices);
        childQuestion.choices = relevantChoices;

        // Reset value if not in new choices
        if (childQuestion.value && !relevantChoices.some(c => c.value === childQuestion.value)) {
            childQuestion.value = null;
        }

        // Save the current state
        saveSurveyProgress(survey);
    }

    const attemptUpdate = (retries = 3) => {
        if (typeof survey === 'undefined' || survey === null) {
            if (retries > 0) {
                console.log(`Waiting for survey... (${retries} attempts left)`);
                setTimeout(() => attemptUpdate(retries - 1), CHOICE_UPDATE_DELAY);
                return;
            }
            document.getElementById("surveyNotDefinedMessage").style.display = "block";
            console.error("Survey not initialized after retries");
            return;
        }

        console.log("Full data received:", data);

        // Store the configuration data for cookie handling
        survey.dynamicConfig = data;

        // First process all standalone and child fields
        Object.entries(data).forEach(([fieldName, fieldData]) => {
            if (fieldData.type === "standalone" || fieldData.type === "child") {
                const targetQuestion = survey.getQuestionByName(fieldName);
                if (!targetQuestion) {
                    console.warn(`Target question not found: ${fieldName}`);
                    return;
                }

                if (fieldData.type === "standalone") {
                    console.log(`Processing standalone field ${fieldName}`);
                    setChoices(targetQuestion, fieldData.choices);
                } else {
                    console.log(`Processing child field ${fieldName}`);
                    processChildField(fieldName, fieldData);
                }
            }
        });

        // Then process parent fields
        Object.entries(data).forEach(([fieldName, fieldData]) => {
            if (fieldData.type === "param_parent" || fieldData.type === "choice_parent") {
                const targetQuestion = survey.getQuestionByName(fieldName);
                if (!targetQuestion) {
                    console.warn(`Target question not found: ${fieldName}`);
                    return;
                }

                console.log(`Setting up ${fieldData.type} for ${fieldName}`);
                setupParentHandler(fieldName, fieldData);

                // Set choices for choice parents
                if (fieldData.type === "choice_parent") {
                    setChoices(targetQuestion, fieldData.choices);
                }
            }
        });

        // Initial update for child fields
        console.log("Running initial child field updates...");
        setTimeout(updateChildFieldsFromParents, CHOICE_UPDATE_DELAY);
    };

    attemptUpdate();
}

// Cookie handling functions
function saveSurveyProgress(survey) {
    try {
        if (!survey || !survey.data) {
            console.warn("Invalid survey instance or data");
            return;
        }

        // Get the actual values for storage
        const dataToStore = { ...survey.data };

        // Add dynamic configuration data
        if (survey.dynamicConfig) {
            dataToStore._dynamicConfig = {
                parentFields: {},
                childChoices: {}
            };

            // Store parent field configurations
            Object.entries(survey.parentConfig || {}).forEach(([parentField, config]) => {
                const parentQuestion = survey.getQuestionByName(parentField);
                if (parentQuestion) {
                    dataToStore._dynamicConfig.parentFields[parentField] = {
                        value: parentQuestion.value,
                        type: config.type,
                        choices: config.choicesList
                    };

                    // Store child field configuration
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

        console.log("Saving survey data:", dataToStore);
        setCookie(COOKIE_NAME, dataToStore, COOKIE_EXPIRATION_DAYS);
    } catch (error) {
        console.error("Error saving survey progress:", error);
    }
}

function restoreDynamicChoices(survey, savedData) {
    try {
        if (!savedData || !savedData._dynamicConfig) {
            console.log("No saved dynamic configuration found");
            return;
        }

        console.log("Restoring dynamic choices:", savedData._dynamicConfig);
        const { parentFields, childChoices } = savedData._dynamicConfig;

        // First restore parent values
        Object.entries(parentFields).forEach(([parentField, config]) => {
            const parentQuestion = survey.getQuestionByName(parentField);
            if (parentQuestion) {
                console.log(`Restoring parent ${parentField}:`, config);

                if (config.type === "choice_parent" && config.choices) {
                    parentQuestion.choices = config.choices;
                }

                if (config.value !== undefined) {
                    parentQuestion.value = config.value;
                }
            }
        });

        // Then restore child choices and values
        Object.entries(childChoices).forEach(([childField, config]) => {
            const childQuestion = survey.getQuestionByName(childField);
            if (childQuestion && config.choices) {
                console.log(`Restoring child ${childField}:`, config);
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

Shiny.addCustomMessageHandler("updateDynamicChoices", updateDynamicChoices);

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
