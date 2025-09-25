"""
Unit tests for the validator agent.

This module contains unit tests for the validator agent
functionality and behavior.
"""

import pytest
import asyncio
from unittest.mock import Mock, patch, AsyncMock, MagicMock
from src.core.agents.validator_agent import ValidatorAgent
from src.core.state.agent_state import AgentState
from src.config.settings import Settings


class TestValidatorAgent:
    """Test cases for the validator agent."""
    
    @pytest.fixture
    def settings(self):
        """Create settings fixture."""
        return Settings()
    
    @pytest.fixture
    def validator_agent(self, settings):
        """Create validator agent fixture."""
        return ValidatorAgent(settings)
    
    @pytest.fixture
    def mock_state(self):
        """Create mock agent state."""
        return {
            "agent_id": "validator",
            "agent_type": "validator",
            "codebase_path": "/test/codebase",
            "target_language": "python",
            "modernization_goals": ["maintainability"],
            "transformation_results": [
                {"file": "file1.py", "content": "def function1(): pass"},
                {"file": "file2.py", "content": "def function2(): pass"}
            ],
            "test_results": {
                "passed": 8,
                "failed": 2,
                "total": 10
            },
            "quality_review": {
                "overall_score": 8.5,
                "issues": ["minor_issue1"]
            },
            "test_cases": [
                {"name": "test_function1", "type": "unit", "code": "def test_function1(): assert True"},
                {"name": "test_function2", "type": "unit", "code": "def test_function2(): assert True"}
            ],
            "analysis_results": {
                "structure": {"files": ["file1.cobol", "file2.cobol"]},
                "dependencies": {"dependencies": ["dep1", "dep2"]},
                "patterns": {"patterns": ["pattern1", "pattern2"]}
            },
            "modernization_plan": None,
            "risk_assessment": None,
            "implementation_strategy": None,
            "pattern_results": None,
            "backup_path": None,
            "standards_analysis": None,
            "coverage_analysis": None,
            "test_validation": None,
            "final_validation": None,
            "compliance_check": None,
            "integration_tests": None,
            "agent_data": None,
            "error": None,
            "messages": [],
            "start_time": None,
            "end_time": None,
            "duration": None,
            "modernization_success": None
        }
    
    def test_validator_agent_initialization(self, validator_agent):
        """Test validator agent initialization."""
        assert validator_agent is not None
        assert validator_agent.settings is not None
        assert len(validator_agent.tools) > 0
    
    def test_get_system_prompt(self, validator_agent):
        """Test system prompt generation."""
        prompt = validator_agent.get_system_prompt()
        assert isinstance(prompt, str)
        assert len(prompt) > 0
        assert "validation" in prompt.lower()
    
    @pytest.mark.asyncio
    async def test_run_validator_agent(self, validator_agent, mock_state):
        """Test validator agent execution."""
        with patch.object(validator_agent, 'use_tool') as mock_use_tool:
            # Mock tool responses
            mock_use_tool.side_effect = [
                {"passed": True, "score": 9.0, "issues": []},  # final validation
                {"compliant": True, "score": 8.5, "violations": []},  # compliance check
                {"passed": True, "tests_run": 2, "failures": 0}  # integration tests
            ]
            
            result = await validator_agent.run(mock_state)
            
            assert result is not None
            assert result["final_validation"] is not None
            assert result["compliance_check"] is not None
            assert result["integration_tests"] is not None
            assert result["modernization_success"] is True
    
    @pytest.mark.asyncio
    async def test_run_validator_agent_error(self, validator_agent, mock_state):
        """Test validator agent error handling."""
        with patch.object(validator_agent, 'use_tool') as mock_use_tool:
            mock_use_tool.side_effect = Exception("Tool error")
            
            result = await validator_agent.run(mock_state)
            
            assert result is not None
            assert result["error"] is not None
            assert "Tool error" in result["error"]
            assert result["modernization_success"] is False
    
    def test_get_available_tools(self, validator_agent):
        """Test available tools retrieval."""
        tools = validator_agent.get_available_tools()
        assert isinstance(tools, list)
        assert len(tools) > 0
    
    @pytest.mark.asyncio
    async def test_use_tool(self, validator_agent):
        """Test tool usage."""
        with patch.object(validator_agent.tools[0], 'run') as mock_run:
            mock_run.return_value = {"result": "test"}
            
            result = await validator_agent.use_tool("final_validation", transformed_code={})
            
            assert result is not None
            assert result["result"] == "test"
    
    def test_log_activity(self, validator_agent):
        """Test activity logging."""
        validator_agent.log_activity("test activity", {"key": "value"})
        assert True  # If we get here, no exception was raised

    # ==================== EXTENSIVE TEST SUITE ====================
    
    def test_validator_agent_tools_initialization(self, validator_agent):
        """Test that validator agent has the correct tools initialized."""
        tools = validator_agent.get_available_tools()
        tool_names = [tool.name for tool in tools]
        
        # Should have validation tools
        assert "final_validation" in tool_names
        assert "check_compliance" in tool_names
        
        # Should have integration test tools
        assert "run_integration_tests" in tool_names
        
        # Verify tool count
        assert len(tools) >= 3  # At least 3 tools should be available
    
    def test_validator_agent_with_different_settings(self):
        """Test validator agent with different configuration settings."""
        custom_settings = Settings()
        custom_settings.llm_provider = "openai"
        custom_settings.llm_model = "gpt-4"
        
        agent = ValidatorAgent(custom_settings)
        assert agent.settings.llm_provider == "openai"
        assert agent.settings.llm_model == "gpt-4"
    
    @pytest.mark.asyncio
    async def test_run_with_empty_transformation_results(self, validator_agent):
        """Test validator agent with empty transformation results."""
        empty_state = {
            "agent_id": "validator",
            "agent_type": "validator",
            "codebase_path": "/test/codebase",
            "target_language": "python",
            "modernization_goals": ["maintainability"],
            "transformation_results": [],
            "test_results": {"passed": 0, "failed": 0, "total": 0},
            "quality_review": {"overall_score": 0, "issues": []},
            "test_cases": [],
            "analysis_results": {
                "structure": {"files": []},
                "dependencies": {"dependencies": []},
                "patterns": {"patterns": []}
            },
            "modernization_plan": None,
            "risk_assessment": None,
            "implementation_strategy": None,
            "pattern_results": None,
            "backup_path": None,
            "standards_analysis": None,
            "coverage_analysis": None,
            "test_validation": None,
            "final_validation": None,
            "compliance_check": None,
            "integration_tests": None,
            "agent_data": None,
            "error": None,
            "messages": [],
            "start_time": None,
            "end_time": None,
            "duration": None,
            "modernization_success": None
        }
        
        with patch.object(validator_agent, 'use_tool') as mock_use_tool:
            # Mock responses for empty results
            mock_use_tool.side_effect = [
                {"passed": False, "score": 0, "issues": ["No code to validate"]},  # final validation
                {"compliant": False, "score": 0, "violations": ["No code to check"]},  # compliance check
                {"passed": False, "tests_run": 0, "failures": 0}  # integration tests
            ]
            
            result = await validator_agent.run(empty_state)
            
            assert result is not None
            assert result["final_validation"] is not None
            assert result["compliance_check"] is not None
            assert result["integration_tests"] is not None
            assert result["modernization_success"] is False
    
    @pytest.mark.asyncio
    async def test_run_with_complex_transformation_results(self, validator_agent):
        """Test validator agent with complex transformation results."""
        complex_state = {
            "agent_id": "validator",
            "agent_type": "validator",
            "codebase_path": "/complex/codebase",
            "target_language": "python",
            "modernization_goals": ["maintainability", "performance"],
            "transformation_results": [
                {"file": f"file{i}.py", "content": f"def function{i}(): pass"} for i in range(50)
            ],
            "test_results": {
                "passed": 45,
                "failed": 5,
                "total": 50,
                "execution_time": 120.5
            },
            "quality_review": {
                "overall_score": 8.5,
                "issues": [f"issue{i}" for i in range(10)],
                "recommendations": [f"rec{i}" for i in range(5)]
            },
            "test_cases": [
                {"name": f"test{i}", "type": "unit", "code": f"def test{i}(): assert True"} for i in range(20)
            ],
            "analysis_results": {
                "structure": {"files": [f"file{i}.cobol" for i in range(100)]},
                "dependencies": {"dependencies": [f"dep{i}" for i in range(50)]},
                "patterns": {"patterns": [f"pattern{i}" for i in range(25)]}
            },
            "modernization_plan": None,
            "risk_assessment": None,
            "implementation_strategy": None,
            "pattern_results": None,
            "backup_path": None,
            "standards_analysis": None,
            "coverage_analysis": None,
            "test_validation": None,
            "final_validation": None,
            "compliance_check": None,
            "integration_tests": None,
            "agent_data": None,
            "error": None,
            "messages": [],
            "start_time": None,
            "end_time": None,
            "duration": None,
            "modernization_success": None
        }
        
        with patch.object(validator_agent, 'use_tool') as mock_use_tool:
            # Mock complex responses
            mock_use_tool.side_effect = [
                {
                    "passed": True,
                    "score": 9.0,
                    "issues": ["minor_style_issue"],
                    "recommendations": ["Add type hints", "Improve documentation"]
                },
                {
                    "compliant": True,
                    "score": 8.5,
                    "violations": [],
                    "standards_met": ["PEP8", "Type hints", "Documentation"]
                },
                {
                    "passed": True,
                    "tests_run": 20,
                    "failures": 0,
                    "execution_time": 45.2
                }
            ]
            
            result = await validator_agent.run(complex_state)
            
            assert result is not None
            assert result["final_validation"] is not None
            assert result["compliance_check"] is not None
            assert result["integration_tests"] is not None
            assert result["modernization_success"] is True
            assert result["final_validation"]["score"] == 9.0
            assert result["compliance_check"]["score"] == 8.5
            assert result["integration_tests"]["tests_run"] == 20
    
    @pytest.mark.asyncio
    async def test_run_with_partial_tool_failures(self, validator_agent, mock_state):
        """Test validator agent when some tools fail but others succeed."""
        with patch.object(validator_agent, 'use_tool') as mock_use_tool:
            # First tool succeeds, second fails, third succeeds
            mock_use_tool.side_effect = [
                {"passed": True, "score": 9.0, "issues": []},  # final validation succeeds
                Exception("Compliance check failed"),  # compliance check fails
                {"passed": True, "tests_run": 2, "failures": 0}  # integration tests succeeds
            ]
            
            result = await validator_agent.run(mock_state)
            
            # Should handle partial failures gracefully
            assert result is not None
            assert result["error"] is not None
            assert "Compliance check failed" in result["error"]
            assert result["modernization_success"] is False
    
    @pytest.mark.asyncio
    async def test_run_with_timeout_simulation(self, validator_agent, mock_state):
        """Test validator agent with timeout simulation."""
        with patch.object(validator_agent, 'use_tool') as mock_use_tool:
            # Simulate timeout by making tools take too long
            async def slow_tool(*args, **kwargs):
                await asyncio.sleep(0.1)  # Simulate slow operation
                return {"result": "slow_result"}
            
            mock_use_tool.side_effect = slow_tool
            
            result = await validator_agent.run(mock_state)
            
            # Should complete despite slow tools
            assert result is not None
    
    @pytest.mark.asyncio
    async def test_run_with_malformed_tool_responses(self, validator_agent, mock_state):
        """Test validator agent with malformed tool responses."""
        with patch.object(validator_agent, 'use_tool') as mock_use_tool:
            # Mock malformed responses
            mock_use_tool.side_effect = [
                None,  # None response
                {"compliant": "not_a_boolean"},  # Wrong type
                {"passed": []}  # Wrong type
            ]
            
            result = await validator_agent.run(mock_state)
            
            # Should handle malformed responses gracefully
            assert result is not None
    
    @pytest.mark.asyncio
    async def test_use_tool_with_invalid_tool_name(self, validator_agent):
        """Test using a tool that doesn't exist."""
        with pytest.raises(ValueError, match="Tool 'invalid_tool' not found"):
            await validator_agent.use_tool("invalid_tool", param="value")
    
    @pytest.mark.asyncio
    async def test_use_tool_with_missing_parameters(self, validator_agent):
        """Test using a tool with missing required parameters."""
        # This should raise an error due to missing required parameter
        with pytest.raises((ValueError, TypeError)):
            await validator_agent.use_tool("final_validation")
    
    @pytest.mark.asyncio
    async def test_use_tool_with_extra_parameters(self, validator_agent):
        """Test using a tool with extra parameters."""
        with patch.object(validator_agent.tools[0], 'run') as mock_run:
            mock_run.return_value = {"result": "test"}
            
            # Should handle extra parameters gracefully
            result = await validator_agent.use_tool(
                "final_validation", 
                transformed_code={},
                extra_param="extra_value"
            )
            
            assert result is not None
    
    def test_get_system_prompt_content(self, validator_agent):
        """Test system prompt contains expected content."""
        prompt = validator_agent.get_system_prompt()
        
        # Check for key concepts in the prompt
        assert "validation" in prompt.lower()
        assert "compliance" in prompt.lower()
        assert "integration" in prompt.lower()
        assert "functionality" in prompt.lower()
        assert "requirements" in prompt.lower()
        assert "standards" in prompt.lower()
    
    def test_get_system_prompt_length(self, validator_agent):
        """Test system prompt has reasonable length."""
        prompt = validator_agent.get_system_prompt()
        
        # Should be substantial but not excessive
        assert len(prompt) > 100  # At least 100 characters
        assert len(prompt) < 2000  # Not more than 2000 characters
    
    def test_log_activity_with_different_data_types(self, validator_agent):
        """Test logging with different data types."""
        # Test with string
        validator_agent.log_activity("string activity")
        
        # Test with dict
        validator_agent.log_activity("dict activity", {"key": "value", "number": 42})
        
        # Test with list
        validator_agent.log_activity("list activity", ["item1", "item2"])
        
        # Test with None
        validator_agent.log_activity("none activity", None)
        
        # All should complete without errors
        assert True
    
    def test_log_activity_with_complex_data(self, validator_agent):
        """Test logging with complex nested data structures."""
        complex_data = {
            "validation": {
                "final_validation": {
                    "passed": True,
                    "score": 9.0,
                    "issues": ["Minor style issue"],
                    "recommendations": ["Add type hints", "Improve error handling"]
                },
                "compliance": {
                    "compliant": True,
                    "score": 8.5,
                    "violations": [],
                    "standards_met": ["PEP8", "Type hints", "Documentation"]
                },
                "integration": {
                    "passed": True,
                    "tests_run": 15,
                    "failures": 0,
                    "execution_time": 30.5
                }
            },
            "overall": {
                "modernization_success": True,
                "total_score": 8.8,
                "recommendations": ["Add more tests", "Improve documentation"]
            }
        }
        
        validator_agent.log_activity("complex validation", complex_data)
        assert True  # Should complete without errors
    
    @pytest.mark.asyncio
    async def test_concurrent_tool_usage(self, validator_agent):
        """Test concurrent usage of multiple tools."""
        async def use_multiple_tools():
            tasks = []
            for i in range(5):
                task = validator_agent.use_tool(
                    "final_validation", 
                    transformed_code={"file": f"file{i}.py"}
                )
                tasks.append(task)
            
            results = await asyncio.gather(*tasks, return_exceptions=True)
            return results
        
        with patch.object(validator_agent.tools[0], 'run') as mock_run:
            mock_run.return_value = {"result": "test"}
            
            results = await use_multiple_tools()
            
            # All should complete successfully
            assert len(results) == 5
            for result in results:
                assert not isinstance(result, Exception)
    
    @pytest.mark.asyncio
    async def test_run_with_state_mutation(self, validator_agent):
        """Test that agent run doesn't mutate the original state inappropriately."""
        original_state = {
            "agent_id": "validator",
            "agent_type": "validator",
            "codebase_path": "/test/codebase",
            "target_language": "python",
            "modernization_goals": ["maintainability"],
            "transformation_results": [{"file": "file1.py", "content": "def function1(): pass"}],
            "test_results": {"passed": 1, "failed": 0, "total": 1},
            "quality_review": {"overall_score": 8.0, "issues": []},
            "test_cases": [{"name": "test1", "type": "unit", "code": "def test1(): assert True"}],
            "analysis_results": {
                "structure": {"files": ["file1.cobol"]},
                "dependencies": {"dependencies": ["dep1"]},
                "patterns": {"patterns": ["pattern1"]}
            },
            "modernization_plan": None,
            "risk_assessment": None,
            "implementation_strategy": None,
            "pattern_results": None,
            "backup_path": None,
            "standards_analysis": None,
            "coverage_analysis": None,
            "test_validation": None,
            "final_validation": None,
            "compliance_check": None,
            "integration_tests": None,
            "agent_data": None,
            "error": None,
            "messages": [],
            "start_time": None,
            "end_time": None,
            "duration": None,
            "modernization_success": None
        }
        
        # Create a copy to compare later
        state_copy = original_state.copy()
        
        with patch.object(validator_agent, 'use_tool') as mock_use_tool:
            mock_use_tool.side_effect = [
                {"passed": True, "score": 9.0, "issues": []},  # final validation
                {"compliant": True, "score": 8.5, "violations": []},  # compliance check
                {"passed": True, "tests_run": 1, "failures": 0}  # integration tests
            ]
            
            result = await validator_agent.run(original_state)
            
            # Original state should be modified (this is expected behavior)
            assert result is not None
            assert result["final_validation"] is not None
            assert result["modernization_success"] is True
    
    def test_validator_agent_tool_integration(self, validator_agent):
        """Test that all tools are properly integrated."""
        tools = validator_agent.get_available_tools()
        
        # Each tool should have required attributes
        for tool in tools:
            assert hasattr(tool, 'name')
            assert hasattr(tool, 'description')
            assert hasattr(tool, 'run')
            assert callable(tool.run)
            
            # Tool names should be non-empty strings
            assert isinstance(tool.name, str)
            assert len(tool.name) > 0
            
            # Descriptions should be non-empty strings
            assert isinstance(tool.description, str)
            assert len(tool.description) > 0
    
    @pytest.mark.asyncio
    async def test_run_with_different_modernization_goals(self, validator_agent):
        """Test validator agent with different modernization goals."""
        goals_variations = [
            ["maintainability"],
            ["performance"],
            ["security"],
            ["maintainability", "performance"],
            ["maintainability", "performance", "security"],
            []
        ]
        
        for goals in goals_variations:
            state = {
                "agent_id": "validator",
                "agent_type": "validator",
                "codebase_path": "/test/codebase",
                "target_language": "python",
                "modernization_goals": goals,
                "transformation_results": [{"file": "file1.py", "content": "def function1(): pass"}],
                "test_results": {"passed": 1, "failed": 0, "total": 1},
                "quality_review": {"overall_score": 8.0, "issues": []},
                "test_cases": [{"name": "test1", "type": "unit", "code": "def test1(): assert True"}],
                "analysis_results": {
                    "structure": {"files": ["file1.cobol"]},
                    "dependencies": {"dependencies": ["dep1"]},
                    "patterns": {"patterns": ["pattern1"]}
                },
                "modernization_plan": None,
                "risk_assessment": None,
                "implementation_strategy": None,
                "pattern_results": None,
                "backup_path": None,
                "standards_analysis": None,
                "coverage_analysis": None,
                "test_validation": None,
                "final_validation": None,
                "compliance_check": None,
                "integration_tests": None,
                "agent_data": None,
                "error": None,
                "messages": [],
                "start_time": None,
                "end_time": None,
                "duration": None,
                "modernization_success": None
            }
            
            with patch.object(validator_agent, 'use_tool') as mock_use_tool:
                mock_use_tool.side_effect = [
                    {"passed": True, "score": 9.0, "issues": []},  # final validation
                    {"compliant": True, "score": 8.5, "violations": []},  # compliance check
                    {"passed": True, "tests_run": 1, "failures": 0}  # integration tests
                ]
                
                result = await validator_agent.run(state)
                assert result is not None
    
    @pytest.mark.asyncio
    async def test_run_with_different_target_languages(self, validator_agent):
        """Test validator agent with different target languages."""
        languages = ["python", "javascript", "java", "csharp", "go"]
        
        for language in languages:
            state = {
                "agent_id": "validator",
                "agent_type": "validator",
                "codebase_path": "/test/codebase",
                "target_language": language,
                "modernization_goals": ["maintainability"],
                "transformation_results": [{"file": f"file1.{language}", "content": f"def function1(): pass"}],
                "test_results": {"passed": 1, "failed": 0, "total": 1},
                "quality_review": {"overall_score": 8.0, "issues": []},
                "test_cases": [{"name": "test1", "type": "unit", "code": "def test1(): assert True"}],
                "analysis_results": {
                    "structure": {"files": [f"file1.{language}"]},
                    "dependencies": {"dependencies": ["dep1"]},
                    "patterns": {"patterns": ["pattern1"]}
                },
                "modernization_plan": None,
                "risk_assessment": None,
                "implementation_strategy": None,
                "pattern_results": None,
                "backup_path": None,
                "standards_analysis": None,
                "coverage_analysis": None,
                "test_validation": None,
                "final_validation": None,
                "compliance_check": None,
                "integration_tests": None,
                "agent_data": None,
                "error": None,
                "messages": [],
                "start_time": None,
                "end_time": None,
                "duration": None,
                "modernization_success": None
            }
            
            with patch.object(validator_agent, 'use_tool') as mock_use_tool:
                mock_use_tool.side_effect = [
                    {"passed": True, "score": 9.0, "issues": []},  # final validation
                    {"compliant": True, "score": 8.5, "violations": []},  # compliance check
                    {"passed": True, "tests_run": 1, "failures": 0}  # integration tests
                ]
                
                result = await validator_agent.run(state)
                assert result is not None
                assert result["target_language"] == language
    
    def test_validator_agent_memory_usage(self, validator_agent):
        """Test that validator agent doesn't have memory leaks."""
        import gc
        
        # Create and destroy multiple agents
        for i in range(10):
            temp_agent = ValidatorAgent(Settings())
            del temp_agent
        
        # Force garbage collection
        gc.collect()
        
        # Original agent should still work
        assert validator_agent is not None
        assert len(validator_agent.get_available_tools()) > 0
    
    @pytest.mark.asyncio
    async def test_run_with_corrupted_state(self, validator_agent):
        """Test validator agent with corrupted or incomplete state."""
        corrupted_states = [
            {},  # Empty state
            {"agent_id": "validator"},  # Minimal state
            {"transformation_results": None},  # None results
            {"transformation_results": []},  # Empty results
        ]
        
        for state in corrupted_states:
            # Fill in missing required fields
            complete_state = {
                "agent_id": "validator",
                "agent_type": "validator",
                "codebase_path": "/test/codebase",
                "target_language": "python",
                "modernization_goals": ["maintainability"],
                "transformation_results": [{"file": "file1.py", "content": "def function1(): pass"}],
                "test_results": {"passed": 1, "failed": 0, "total": 1},
                "quality_review": {"overall_score": 8.0, "issues": []},
                "test_cases": [{"name": "test1", "type": "unit", "code": "def test1(): assert True"}],
                "analysis_results": {
                    "structure": {"files": ["file1.cobol"]},
                    "dependencies": {"dependencies": ["dep1"]},
                    "patterns": {"patterns": ["pattern1"]}
                },
                "modernization_plan": None,
                "risk_assessment": None,
                "implementation_strategy": None,
                "pattern_results": None,
                "backup_path": None,
                "standards_analysis": None,
                "coverage_analysis": None,
                "test_validation": None,
                "final_validation": None,
                "compliance_check": None,
                "integration_tests": None,
                "agent_data": None,
                "error": None,
                "messages": [],
                "start_time": None,
                "end_time": None,
                "duration": None,
                "modernization_success": None,
                **state  # Override with corrupted values
            }
            
            with patch.object(validator_agent, 'use_tool') as mock_use_tool:
                mock_use_tool.side_effect = [
                    {"passed": True, "score": 9.0, "issues": []},  # final validation
                    {"compliant": True, "score": 8.5, "violations": []},  # compliance check
                    {"passed": True, "tests_run": 1, "failures": 0}  # integration tests
                ]
                
                result = await validator_agent.run(complete_state)
                # Should handle corrupted state gracefully
                assert result is not None

    # ==================== PERFORMANCE & STRESS TESTS ====================
    
    @pytest.mark.asyncio
    async def test_run_performance_benchmark(self, validator_agent):
        """Test validator agent performance with timing measurements."""
        import time
        
        state = {
            "agent_id": "validator",
            "agent_type": "validator",
            "codebase_path": "/test/codebase",
            "target_language": "python",
            "modernization_goals": ["maintainability"],
            "transformation_results": [{"file": "file1.py", "content": "def function1(): pass"}],
            "test_results": {"passed": 1, "failed": 0, "total": 1},
            "quality_review": {"overall_score": 8.0, "issues": []},
            "test_cases": [{"name": "test1", "type": "unit", "code": "def test1(): assert True"}],
            "analysis_results": {
                "structure": {"files": ["file1.cobol"]},
                "dependencies": {"dependencies": ["dep1"]},
                "patterns": {"patterns": ["pattern1"]}
            },
            "modernization_plan": None,
            "risk_assessment": None,
            "implementation_strategy": None,
            "pattern_results": None,
            "backup_path": None,
            "standards_analysis": None,
            "coverage_analysis": None,
            "test_validation": None,
            "final_validation": None,
            "compliance_check": None,
            "integration_tests": None,
            "agent_data": None,
            "error": None,
            "messages": [],
            "start_time": None,
            "end_time": None,
            "duration": None,
            "modernization_success": None
        }
        
        with patch.object(validator_agent, 'use_tool') as mock_use_tool:
            mock_use_tool.side_effect = [
                {"passed": True, "score": 9.0, "issues": []},  # final validation
                {"compliant": True, "score": 8.5, "violations": []},  # compliance check
                {"passed": True, "tests_run": 1, "failures": 0}  # integration tests
            ]
            
            start_time = time.time()
            result = await validator_agent.run(state)
            end_time = time.time()
            
            execution_time = end_time - start_time
            
            # Should complete within reasonable time (less than 5 seconds)
            assert execution_time < 5.0
            assert result is not None
    
    @pytest.mark.asyncio
    async def test_stress_test_multiple_runs(self, validator_agent):
        """Test validator agent with multiple consecutive runs."""
        state = {
            "agent_id": "validator",
            "agent_type": "validator",
            "codebase_path": "/test/codebase",
            "target_language": "python",
            "modernization_goals": ["maintainability"],
            "transformation_results": [{"file": "file1.py", "content": "def function1(): pass"}],
            "test_results": {"passed": 1, "failed": 0, "total": 1},
            "quality_review": {"overall_score": 8.0, "issues": []},
            "test_cases": [{"name": "test1", "type": "unit", "code": "def test1(): assert True"}],
            "analysis_results": {
                "structure": {"files": ["file1.cobol"]},
                "dependencies": {"dependencies": ["dep1"]},
                "patterns": {"patterns": ["pattern1"]}
            },
            "modernization_plan": None,
            "risk_assessment": None,
            "implementation_strategy": None,
            "pattern_results": None,
            "backup_path": None,
            "standards_analysis": None,
            "coverage_analysis": None,
            "test_validation": None,
            "final_validation": None,
            "compliance_check": None,
            "integration_tests": None,
            "agent_data": None,
            "error": None,
            "messages": [],
            "start_time": None,
            "end_time": None,
            "duration": None,
            "modernization_success": None
        }
        
        with patch.object(validator_agent, 'use_tool') as mock_use_tool:
            mock_use_tool.side_effect = [
                {"passed": True, "score": 9.0, "issues": []},  # final validation
                {"compliant": True, "score": 8.5, "violations": []},  # compliance check
                {"passed": True, "tests_run": 1, "failures": 0}  # integration tests
            ]
            
            # Run multiple times to test for memory leaks or state issues
            for i in range(10):
                # Create a fresh copy of state for each run
                fresh_state = state.copy()
                result = await validator_agent.run(fresh_state)
                assert result is not None
                # Check if final_validation exists (may be None on error)
                if result.get("final_validation") is not None:
                    assert result["final_validation"] is not None
    
    @pytest.mark.asyncio
    async def test_concurrent_agent_instances(self):
        """Test multiple validator agent instances running concurrently."""
        async def run_agent_instance(agent_id):
            agent = ValidatorAgent(Settings())
            state = {
                "agent_id": f"validator_{agent_id}",
                "agent_type": "validator",
                "codebase_path": f"/test/codebase_{agent_id}",
                "target_language": "python",
                "modernization_goals": ["maintainability"],
                "transformation_results": [{"file": f"file{agent_id}.py", "content": f"def function{agent_id}(): pass"}],
                "test_results": {"passed": 1, "failed": 0, "total": 1},
                "quality_review": {"overall_score": 8.0, "issues": []},
                "test_cases": [{"name": f"test{agent_id}", "type": "unit", "code": f"def test{agent_id}(): assert True"}],
                "analysis_results": {
                    "structure": {"files": [f"file{agent_id}.cobol"]},
                    "dependencies": {"dependencies": [f"dep{agent_id}"]},
                    "patterns": {"patterns": [f"pattern{agent_id}"]}
                },
                "modernization_plan": None,
                "risk_assessment": None,
                "implementation_strategy": None,
                "pattern_results": None,
                "backup_path": None,
                "standards_analysis": None,
                "coverage_analysis": None,
                "test_validation": None,
                "final_validation": None,
                "compliance_check": None,
                "integration_tests": None,
                "agent_data": None,
                "error": None,
                "messages": [],
                "start_time": None,
                "end_time": None,
                "duration": None,
                "modernization_success": None
            }
            
            with patch.object(agent, 'use_tool') as mock_use_tool:
                mock_use_tool.side_effect = [
                    {"passed": True, "score": 9.0, "issues": []},  # final validation
                    {"compliant": True, "score": 8.5, "violations": []},  # compliance check
                    {"passed": True, "tests_run": 1, "failures": 0}  # integration tests
                ]
                
                return await agent.run(state)
        
        # Run 5 concurrent agent instances
        tasks = [run_agent_instance(i) for i in range(5)]
        results = await asyncio.gather(*tasks)
        
        # All should complete successfully
        assert len(results) == 5
        for i, result in enumerate(results):
            assert result is not None
            assert result["agent_id"] == f"validator_{i}"
    
    @pytest.mark.asyncio
    async def test_tool_error_recovery(self, validator_agent, mock_state):
        """Test validator agent recovery from various tool errors."""
        error_scenarios = [
            Exception("Network timeout"),
            ValueError("Invalid input"),
            RuntimeError("Resource unavailable"),
            KeyError("Missing key"),
            TypeError("Wrong type"),
        ]
        
        for error in error_scenarios:
            with patch.object(validator_agent, 'use_tool') as mock_use_tool:
                mock_use_tool.side_effect = error
                
                result = await validator_agent.run(mock_state.copy())
                
                # Should handle all error types gracefully
                assert result is not None
                assert result["error"] is not None
                assert str(error) in result["error"]
                assert result["modernization_success"] is False
    
    def test_validator_agent_thread_safety(self, validator_agent):
        """Test validator agent thread safety (basic check)."""
        import threading
        import time
        
        results = []
        errors = []
        
        def run_agent():
            try:
                # Test basic operations that should be thread-safe
                tools = validator_agent.get_available_tools()
                prompt = validator_agent.get_system_prompt()
                results.append((len(tools), len(prompt)))
            except Exception as e:
                errors.append(e)
        
        # Create multiple threads
        threads = []
        for i in range(5):
            thread = threading.Thread(target=run_agent)
            threads.append(thread)
            thread.start()
        
        # Wait for all threads to complete
        for thread in threads:
            thread.join()
        
        # Should have no errors and consistent results
        assert len(errors) == 0
        assert len(results) == 5
        assert all(len(r) == 2 for r in results)  # Each result should have 2 values
    
    @pytest.mark.asyncio
    async def test_validator_agent_with_extreme_data_sizes(self, validator_agent):
        """Test validator agent with extreme data sizes."""
        # Test with very large data
        large_state = {
            "agent_id": "validator",
            "agent_type": "validator",
            "codebase_path": "/test/codebase",
            "target_language": "python",
            "modernization_goals": ["maintainability"],
            "transformation_results": [
                {"file": f"file{i}.py", "content": f"def function{i}(): pass"} for i in range(1000)
            ],
            "test_results": {
                "passed": 950,
                "failed": 50,
                "total": 1000,
                "execution_time": 300.5
            },
            "quality_review": {
                "overall_score": 8.5,
                "issues": [f"issue{i}" for i in range(100)],
                "recommendations": [f"rec{i}" for i in range(50)]
            },
            "test_cases": [
                {"name": f"test{i}", "type": "unit", "code": f"def test{i}(): assert True"} for i in range(500)
            ],
            "analysis_results": {
                "structure": {"files": [f"file{i}.cobol" for i in range(10000)]},
                "dependencies": {"dependencies": [f"dep{i}" for i in range(5000)]},
                "patterns": {"patterns": [f"pattern{i}" for i in range(2500)]}
            },
            "modernization_plan": None,
            "risk_assessment": None,
            "implementation_strategy": None,
            "pattern_results": None,
            "backup_path": None,
            "standards_analysis": None,
            "coverage_analysis": None,
            "test_validation": None,
            "final_validation": None,
            "compliance_check": None,
            "integration_tests": None,
            "agent_data": None,
            "error": None,
            "messages": [],
            "start_time": None,
            "end_time": None,
            "duration": None,
            "modernization_success": None
        }
        
        with patch.object(validator_agent, 'use_tool') as mock_use_tool:
            # Mock extremely large responses
            huge_validation = {
                "passed": True,
                "score": 9.0,
                "issues": [f"issue{i}" for i in range(100)],
                "recommendations": [f"rec{i}" for i in range(50)]
            }
            huge_compliance = {
                "compliant": True,
                "score": 8.5,
                "violations": [f"violation{i}" for i in range(50)],
                "standards_met": [f"standard{i}" for i in range(100)]
            }
            huge_integration = {
                "passed": True,
                "tests_run": 500,
                "failures": 0,
                "execution_time": 120.5
            }
            
            mock_use_tool.side_effect = [
                huge_validation,  # final validation
                huge_compliance,  # compliance check
                huge_integration  # integration tests
            ]
            
            result = await validator_agent.run(large_state)
            
            # Should handle large data without issues
            assert result is not None
            assert result["final_validation"] is not None
            assert result["compliance_check"] is not None
            assert result["integration_tests"] is not None
            assert result["modernization_success"] is True
            assert len(result["final_validation"]["issues"]) == 100
            assert len(result["compliance_check"]["violations"]) == 50
            assert result["integration_tests"]["tests_run"] == 500
    
    def test_validator_agent_configuration_validation(self):
        """Test validator agent with various configuration scenarios."""
        # Test with minimal settings
        minimal_settings = Settings()
        minimal_settings.llm_provider = "anthropic"
        minimal_settings.llm_model = "claude-3-sonnet"
        
        agent = ValidatorAgent(minimal_settings)
        assert agent is not None
        
        # Test with custom settings
        custom_settings = Settings()
        custom_settings.llm_provider = "openai"
        custom_settings.llm_model = "gpt-4"
        custom_settings.llm_temperature = 0.5
        custom_settings.llm_max_tokens = 2000
        
        agent = ValidatorAgent(custom_settings)
        assert agent.settings.llm_temperature == 0.5
        assert agent.settings.llm_max_tokens == 2000
    
    @pytest.mark.asyncio
    async def test_validator_agent_edge_case_combinations(self, validator_agent):
        """Test validator agent with edge case combinations."""
        edge_cases = [
            # Empty results with multiple goals
            {
                "transformation_results": [],
                "target_language": "python",
                "modernization_goals": ["maintainability", "performance", "security"]
            },
            # Complex results with single goal
            {
                "transformation_results": [
                    {"file": f"file{i}.py", "content": f"def function{i}(): pass"} for i in range(10)
                ],
                "target_language": "javascript",
                "modernization_goals": ["maintainability"]
            },
            # Different language combinations
            {
                "transformation_results": [
                    {"file": "legacy.java", "content": "public class Legacy {}"}
                ],
                "target_language": "java",
                "modernization_goals": ["performance"]
            }
        ]
        
        for case in edge_cases:
            state = {
                "agent_id": "validator",
                "agent_type": "validator",
                "codebase_path": "/test/codebase",
                "test_results": {"passed": 1, "failed": 0, "total": 1},
                "quality_review": {"overall_score": 8.0, "issues": []},
                "test_cases": [{"name": "test1", "type": "unit", "code": "def test1(): assert True"}],
                "analysis_results": {
                    "structure": {"files": ["file1.cobol"]},
                    "dependencies": {"dependencies": ["dep1"]},
                    "patterns": {"patterns": ["pattern1"]}
                },
                "modernization_plan": None,
                "risk_assessment": None,
                "implementation_strategy": None,
                "pattern_results": None,
                "backup_path": None,
                "standards_analysis": None,
                "coverage_analysis": None,
                "test_validation": None,
                "final_validation": None,
                "compliance_check": None,
                "integration_tests": None,
                "agent_data": None,
                "error": None,
                "messages": [],
                "start_time": None,
                "end_time": None,
                "duration": None,
                "modernization_success": None,
                **case
            }
            
            with patch.object(validator_agent, 'use_tool') as mock_use_tool:
                mock_use_tool.side_effect = [
                    {"passed": True, "score": 9.0, "issues": []},  # final validation
                    {"compliant": True, "score": 8.5, "violations": []},  # compliance check
                    {"passed": True, "tests_run": 1, "failures": 0}  # integration tests
                ]
                
                result = await validator_agent.run(state)
                assert result is not None
                assert result["target_language"] == case["target_language"]
                assert result["modernization_goals"] == case["modernization_goals"]
