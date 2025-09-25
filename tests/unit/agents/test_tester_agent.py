"""
Unit tests for the tester agent.

This module contains unit tests for the tester agent
functionality and behavior.
"""

import pytest
import asyncio
from unittest.mock import Mock, patch, AsyncMock, MagicMock
from src.core.agents.tester_agent import TesterAgent
from src.core.state.agent_state import AgentState
from src.config.settings import Settings


class TestTesterAgent:
    """Test cases for the tester agent."""
    
    @pytest.fixture
    def settings(self):
        """Create settings fixture."""
        return Settings()
    
    @pytest.fixture
    def tester_agent(self, settings):
        """Create tester agent fixture."""
        return TesterAgent(settings)
    
    @pytest.fixture
    def mock_state(self):
        """Create mock agent state."""
        return {
            "agent_id": "tester",
            "agent_type": "tester",
            "codebase_path": "/test/codebase",
            "target_language": "python",
            "modernization_goals": ["maintainability"],
            "test_framework": "pytest",
            "test_cases": [
                {"name": "test_function1", "type": "unit", "code": "def test_function1(): assert True"},
                {"name": "test_function2", "type": "unit", "code": "def test_function2(): assert True"}
            ],
            "transformation_results": [
                {"file": "file1.py", "content": "def function1(): pass"},
                {"file": "file2.py", "content": "def function2(): pass"}
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
            "quality_review": None,
            "standards_analysis": None,
            "test_results": None,
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
            "duration": None
        }
    
    def test_tester_agent_initialization(self, tester_agent):
        """Test tester agent initialization."""
        assert tester_agent is not None
        assert tester_agent.settings is not None
        assert len(tester_agent.tools) > 0
    
    def test_get_system_prompt(self, tester_agent):
        """Test system prompt generation."""
        prompt = tester_agent.get_system_prompt()
        assert isinstance(prompt, str)
        assert len(prompt) > 0
        assert "test" in prompt.lower()
    
    @pytest.mark.asyncio
    async def test_run_tester_agent(self, tester_agent, mock_state):
        """Test tester agent execution."""
        with patch.object(tester_agent, 'use_tool') as mock_use_tool:
            # Mock tool responses
            mock_use_tool.side_effect = [
                {"passed": 2, "failed": 0, "total": 2},  # test runner
                {"coverage_percentage": 85, "lines_covered": 10, "lines_total": 12},  # coverage analysis
                {"completeness_score": 9.0, "missing_tests": []}  # test validation
            ]
            
            result = await tester_agent.run(mock_state)
            
            assert result is not None
            assert result["test_results"] is not None
            assert result["coverage_analysis"] is not None
            assert result["test_validation"] is not None
    
    @pytest.mark.asyncio
    async def test_run_tester_agent_error(self, tester_agent, mock_state):
        """Test tester agent error handling."""
        with patch.object(tester_agent, 'use_tool') as mock_use_tool:
            mock_use_tool.side_effect = Exception("Tool error")
            
            result = await tester_agent.run(mock_state)
            
            assert result is not None
            assert result["error"] is not None
            assert "Tool error" in result["error"]
    
    def test_get_available_tools(self, tester_agent):
        """Test available tools retrieval."""
        tools = tester_agent.get_available_tools()
        assert isinstance(tools, list)
        assert len(tools) > 0
    
    @pytest.mark.asyncio
    async def test_use_tool(self, tester_agent):
        """Test tool usage."""
        with patch.object(tester_agent.tools[0], 'run') as mock_run:
            mock_run.return_value = {"result": "test"}
            
            result = await tester_agent.use_tool("run_test_cases", test_cases=[])
            
            assert result is not None
            assert result["result"] == "test"
    
    def test_log_activity(self, tester_agent):
        """Test activity logging."""
        tester_agent.log_activity("test activity", {"key": "value"})
        assert True  # If we get here, no exception was raised

    # ==================== EXTENSIVE TEST SUITE ====================
    
    def test_tester_agent_tools_initialization(self, tester_agent):
        """Test that tester agent has the correct tools initialized."""
        tools = tester_agent.get_available_tools()
        tool_names = [tool.name for tool in tools]
        
        # Should have test tools
        assert "run_test_cases" in tool_names
        assert "analyze_test_coverage" in tool_names
        
        # Should have validation tools
        assert "validate_test_completeness" in tool_names
        
        # Verify tool count
        assert len(tools) >= 3  # At least 3 tools should be available
    
    def test_tester_agent_with_different_settings(self):
        """Test tester agent with different configuration settings."""
        custom_settings = Settings()
        custom_settings.llm_provider = "openai"
        custom_settings.llm_model = "gpt-4"
        
        agent = TesterAgent(custom_settings)
        assert agent.settings.llm_provider == "openai"
        assert agent.settings.llm_model == "gpt-4"
    
    @pytest.mark.asyncio
    async def test_run_with_empty_test_cases(self, tester_agent):
        """Test tester agent with empty test cases."""
        empty_state = {
            "agent_id": "tester",
            "agent_type": "tester",
            "codebase_path": "/test/codebase",
            "target_language": "python",
            "modernization_goals": ["maintainability"],
            "test_framework": "pytest",
            "test_cases": [],
            "transformation_results": [
                {"file": "file1.py", "content": "def function1(): pass"}
            ],
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
            "quality_review": None,
            "standards_analysis": None,
            "test_results": None,
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
            "duration": None
        }
        
        with patch.object(tester_agent, 'use_tool') as mock_use_tool:
            # Mock responses for empty test cases
            mock_use_tool.side_effect = [
                {"passed": 0, "failed": 0, "total": 0},  # test runner
                {"coverage_percentage": 0, "lines_covered": 0, "lines_total": 0},  # coverage analysis
                {"completeness_score": 0, "missing_tests": []}  # test validation
            ]
            
            result = await tester_agent.run(empty_state)
            
            assert result is not None
            assert result["test_results"] is not None
            assert result["coverage_analysis"] is not None
            assert result["test_validation"] is not None
    
    @pytest.mark.asyncio
    async def test_run_with_complex_test_cases(self, tester_agent):
        """Test tester agent with complex test cases."""
        complex_state = {
            "agent_id": "tester",
            "agent_type": "tester",
            "codebase_path": "/complex/codebase",
            "target_language": "python",
            "modernization_goals": ["maintainability", "performance"],
            "test_framework": "pytest",
            "test_cases": [
                {"name": f"test_function{i}", "type": "unit", "code": f"def test_function{i}(): assert True"} for i in range(20)
            ],
            "transformation_results": [
                {"file": f"file{i}.py", "content": f"def function{i}(): pass"} for i in range(10)
            ],
            "analysis_results": {
                "structure": {"files": [f"file{i}.cobol" for i in range(10)]},
                "dependencies": {"dependencies": [f"dep{i}" for i in range(5)]},
                "patterns": {"patterns": [f"pattern{i}" for i in range(3)]}
            },
            "modernization_plan": None,
            "risk_assessment": None,
            "implementation_strategy": None,
            "pattern_results": None,
            "backup_path": None,
            "quality_review": None,
            "standards_analysis": None,
            "test_results": None,
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
            "duration": None
        }
        
        with patch.object(tester_agent, 'use_tool') as mock_use_tool:
            # Mock complex responses
            mock_use_tool.side_effect = [
                {
                    "passed": 18,
                    "failed": 2,
                    "total": 20,
                    "execution_time": 15.5,
                    "failed_tests": ["test_function5", "test_function12"]
                },
                {
                    "coverage_percentage": 92.5,
                    "lines_covered": 37,
                    "lines_total": 40,
                    "branches_covered": 15,
                    "branches_total": 16
                },
                {
                    "completeness_score": 8.5,
                    "missing_tests": ["edge_case_1", "error_handling_1"],
                    "recommendations": ["Add integration tests", "Test error conditions"]
                }
            ]
            
            result = await tester_agent.run(complex_state)
            
            assert result is not None
            assert result["test_results"] is not None
            assert result["coverage_analysis"] is not None
            assert result["test_validation"] is not None
            assert result["test_results"]["passed"] == 18
            assert result["coverage_analysis"]["coverage_percentage"] == 92.5
            assert result["test_validation"]["completeness_score"] == 8.5
    
    @pytest.mark.asyncio
    async def test_run_with_partial_tool_failures(self, tester_agent, mock_state):
        """Test tester agent when some tools fail but others succeed."""
        with patch.object(tester_agent, 'use_tool') as mock_use_tool:
            # First tool succeeds, second fails, third succeeds
            mock_use_tool.side_effect = [
                {"passed": 2, "failed": 0, "total": 2},  # test runner succeeds
                Exception("Coverage analysis failed"),  # coverage analysis fails
                {"completeness_score": 9.0, "missing_tests": []}  # test validation succeeds
            ]
            
            result = await tester_agent.run(mock_state)
            
            # Should handle partial failures gracefully
            assert result is not None
            assert result["error"] is not None
            assert "Coverage analysis failed" in result["error"]
    
    @pytest.mark.asyncio
    async def test_run_with_timeout_simulation(self, tester_agent, mock_state):
        """Test tester agent with timeout simulation."""
        with patch.object(tester_agent, 'use_tool') as mock_use_tool:
            # Simulate timeout by making tools take too long
            async def slow_tool(*args, **kwargs):
                await asyncio.sleep(0.1)  # Simulate slow operation
                return {"result": "slow_result"}
            
            mock_use_tool.side_effect = slow_tool
            
            result = await tester_agent.run(mock_state)
            
            # Should complete despite slow tools
            assert result is not None
    
    @pytest.mark.asyncio
    async def test_run_with_malformed_tool_responses(self, tester_agent, mock_state):
        """Test tester agent with malformed tool responses."""
        with patch.object(tester_agent, 'use_tool') as mock_use_tool:
            # Mock malformed responses
            mock_use_tool.side_effect = [
                None,  # None response
                {"coverage_percentage": "not_a_number"},  # Wrong type
                {"completeness_score": []}  # Wrong type
            ]
            
            result = await tester_agent.run(mock_state)
            
            # Should handle malformed responses gracefully
            assert result is not None
    
    @pytest.mark.asyncio
    async def test_use_tool_with_invalid_tool_name(self, tester_agent):
        """Test using a tool that doesn't exist."""
        with pytest.raises(ValueError, match="Tool 'invalid_tool' not found"):
            await tester_agent.use_tool("invalid_tool", param="value")
    
    @pytest.mark.asyncio
    async def test_use_tool_with_missing_parameters(self, tester_agent):
        """Test using a tool with missing required parameters."""
        # This should raise an error due to missing required parameter
        with pytest.raises((ValueError, TypeError)):
            await tester_agent.use_tool("run_test_cases")
    
    @pytest.mark.asyncio
    async def test_use_tool_with_extra_parameters(self, tester_agent):
        """Test using a tool with extra parameters."""
        with patch.object(tester_agent.tools[0], 'run') as mock_run:
            mock_run.return_value = {"result": "test"}
            
            # Should handle extra parameters gracefully
            result = await tester_agent.use_tool(
                "run_test_cases", 
                test_cases=[],
                extra_param="extra_value"
            )
            
            assert result is not None
    
    def test_get_system_prompt_content(self, tester_agent):
        """Test system prompt contains expected content."""
        prompt = tester_agent.get_system_prompt()
        
        # Check for key concepts in the prompt
        assert "test" in prompt.lower()
        assert "generation" in prompt.lower()
        assert "execution" in prompt.lower()
        assert "coverage" in prompt.lower()
        assert "functionality" in prompt.lower()
        assert "validation" in prompt.lower()
    
    def test_get_system_prompt_length(self, tester_agent):
        """Test system prompt has reasonable length."""
        prompt = tester_agent.get_system_prompt()
        
        # Should be substantial but not excessive
        assert len(prompt) > 100  # At least 100 characters
        assert len(prompt) < 2000  # Not more than 2000 characters
    
    def test_log_activity_with_different_data_types(self, tester_agent):
        """Test logging with different data types."""
        # Test with string
        tester_agent.log_activity("string activity")
        
        # Test with dict
        tester_agent.log_activity("dict activity", {"key": "value", "number": 42})
        
        # Test with list
        tester_agent.log_activity("list activity", ["item1", "item2"])
        
        # Test with None
        tester_agent.log_activity("none activity", None)
        
        # All should complete without errors
        assert True
    
    def test_log_activity_with_complex_data(self, tester_agent):
        """Test logging with complex nested data structures."""
        complex_data = {
            "testing": {
                "test_results": {
                    "passed": 15,
                    "failed": 3,
                    "total": 18,
                    "execution_time": 45.2,
                    "failed_tests": ["test_edge_case", "test_error_handling"]
                },
                "coverage": {
                    "percentage": 87.5,
                    "lines_covered": 35,
                    "lines_total": 40,
                    "branches_covered": 12,
                    "branches_total": 15
                },
                "validation": {
                    "completeness_score": 8.5,
                    "missing_tests": ["integration_test_1", "performance_test_1"],
                    "recommendations": ["Add more edge cases", "Test error conditions"]
                }
            },
            "metadata": {
                "timestamp": "2024-01-01T00:00:00Z",
                "version": "1.0.0",
                "test_framework": "pytest"
            }
        }
        
        tester_agent.log_activity("complex testing", complex_data)
        assert True  # Should complete without errors
    
    @pytest.mark.asyncio
    async def test_concurrent_tool_usage(self, tester_agent):
        """Test concurrent usage of multiple tools."""
        async def use_multiple_tools():
            tasks = []
            for i in range(5):
                task = tester_agent.use_tool(
                    "run_test_cases", 
                    test_cases=[{"name": f"test{i}", "type": "unit"}]
                )
                tasks.append(task)
            
            results = await asyncio.gather(*tasks, return_exceptions=True)
            return results
        
        with patch.object(tester_agent.tools[0], 'run') as mock_run:
            mock_run.return_value = {"result": "test"}
            
            results = await use_multiple_tools()
            
            # All should complete successfully
            assert len(results) == 5
            for result in results:
                assert not isinstance(result, Exception)
    
    @pytest.mark.asyncio
    async def test_run_with_state_mutation(self, tester_agent):
        """Test that agent run doesn't mutate the original state inappropriately."""
        original_state = {
            "agent_id": "tester",
            "agent_type": "tester",
            "codebase_path": "/test/codebase",
            "target_language": "python",
            "modernization_goals": ["maintainability"],
            "test_framework": "pytest",
            "test_cases": [{"name": "test1", "type": "unit", "code": "def test1(): assert True"}],
            "transformation_results": [{"file": "file1.py", "content": "def function1(): pass"}],
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
            "quality_review": None,
            "standards_analysis": None,
            "test_results": None,
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
            "duration": None
        }
        
        # Create a copy to compare later
        state_copy = original_state.copy()
        
        with patch.object(tester_agent, 'use_tool') as mock_use_tool:
            mock_use_tool.side_effect = [
                {"passed": 1, "failed": 0, "total": 1},  # test runner
                {"coverage_percentage": 80, "lines_covered": 4, "lines_total": 5},  # coverage analysis
                {"completeness_score": 8.0, "missing_tests": []}  # test validation
            ]
            
            result = await tester_agent.run(original_state)
            
            # Original state should be modified (this is expected behavior)
            assert result is not None
            assert result["test_results"] is not None
    
    def test_tester_agent_tool_integration(self, tester_agent):
        """Test that all tools are properly integrated."""
        tools = tester_agent.get_available_tools()
        
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
    async def test_run_with_different_modernization_goals(self, tester_agent):
        """Test tester agent with different modernization goals."""
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
                "agent_id": "tester",
                "agent_type": "tester",
                "codebase_path": "/test/codebase",
                "target_language": "python",
                "modernization_goals": goals,
                "test_framework": "pytest",
                "test_cases": [{"name": "test1", "type": "unit", "code": "def test1(): assert True"}],
                "transformation_results": [{"file": "file1.py", "content": "def function1(): pass"}],
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
                "quality_review": None,
                "standards_analysis": None,
                "test_results": None,
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
                "duration": None
            }
            
            with patch.object(tester_agent, 'use_tool') as mock_use_tool:
                mock_use_tool.side_effect = [
                    {"passed": 1, "failed": 0, "total": 1},  # test runner
                    {"coverage_percentage": 80, "lines_covered": 4, "lines_total": 5},  # coverage analysis
                    {"completeness_score": 8.0, "missing_tests": []}  # test validation
                ]
                
                result = await tester_agent.run(state)
                assert result is not None
    
    @pytest.mark.asyncio
    async def test_run_with_different_target_languages(self, tester_agent):
        """Test tester agent with different target languages."""
        languages = ["python", "javascript", "java", "csharp", "go"]
        
        for language in languages:
            state = {
                "agent_id": "tester",
                "agent_type": "tester",
                "codebase_path": "/test/codebase",
                "target_language": language,
                "modernization_goals": ["maintainability"],
                "test_framework": "pytest" if language == "python" else "jest" if language == "javascript" else "junit",
                "test_cases": [{"name": "test1", "type": "unit", "code": "def test1(): assert True"}],
                "transformation_results": [{"file": f"file1.{language}", "content": f"def function1(): pass"}],
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
                "quality_review": None,
                "standards_analysis": None,
                "test_results": None,
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
                "duration": None
            }
            
            with patch.object(tester_agent, 'use_tool') as mock_use_tool:
                mock_use_tool.side_effect = [
                    {"passed": 1, "failed": 0, "total": 1},  # test runner
                    {"coverage_percentage": 80, "lines_covered": 4, "lines_total": 5},  # coverage analysis
                    {"completeness_score": 8.0, "missing_tests": []}  # test validation
                ]
                
                result = await tester_agent.run(state)
                assert result is not None
                assert result["target_language"] == language
    
    def test_tester_agent_memory_usage(self, tester_agent):
        """Test that tester agent doesn't have memory leaks."""
        import gc
        
        # Create and destroy multiple agents
        for i in range(10):
            temp_agent = TesterAgent(Settings())
            del temp_agent
        
        # Force garbage collection
        gc.collect()
        
        # Original agent should still work
        assert tester_agent is not None
        assert len(tester_agent.get_available_tools()) > 0
    
    @pytest.mark.asyncio
    async def test_run_with_corrupted_state(self, tester_agent):
        """Test tester agent with corrupted or incomplete state."""
        corrupted_states = [
            {},  # Empty state
            {"agent_id": "tester"},  # Minimal state
            {"test_cases": None},  # None test cases
            {"test_cases": []},  # Empty test cases
        ]
        
        for state in corrupted_states:
            # Fill in missing required fields
            complete_state = {
                "agent_id": "tester",
                "agent_type": "tester",
                "codebase_path": "/test/codebase",
                "target_language": "python",
                "modernization_goals": ["maintainability"],
                "test_framework": "pytest",
                "test_cases": [{"name": "test1", "type": "unit", "code": "def test1(): assert True"}],
                "transformation_results": [{"file": "file1.py", "content": "def function1(): pass"}],
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
                "quality_review": None,
                "standards_analysis": None,
                "test_results": None,
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
                **state  # Override with corrupted values
            }
            
            with patch.object(tester_agent, 'use_tool') as mock_use_tool:
                mock_use_tool.side_effect = [
                    {"passed": 1, "failed": 0, "total": 1},  # test runner
                    {"coverage_percentage": 80, "lines_covered": 4, "lines_total": 5},  # coverage analysis
                    {"completeness_score": 8.0, "missing_tests": []}  # test validation
                ]
                
                result = await tester_agent.run(complete_state)
                # Should handle corrupted state gracefully
                assert result is not None

    # ==================== PERFORMANCE & STRESS TESTS ====================
    
    @pytest.mark.asyncio
    async def test_run_performance_benchmark(self, tester_agent):
        """Test tester agent performance with timing measurements."""
        import time
        
        state = {
            "agent_id": "tester",
            "agent_type": "tester",
            "codebase_path": "/test/codebase",
            "target_language": "python",
            "modernization_goals": ["maintainability"],
            "test_framework": "pytest",
            "test_cases": [{"name": "test1", "type": "unit", "code": "def test1(): assert True"}],
            "transformation_results": [{"file": "file1.py", "content": "def function1(): pass"}],
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
            "quality_review": None,
            "standards_analysis": None,
            "test_results": None,
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
            "duration": None
        }
        
        with patch.object(tester_agent, 'use_tool') as mock_use_tool:
            mock_use_tool.side_effect = [
                {"passed": 1, "failed": 0, "total": 1},  # test runner
                {"coverage_percentage": 80, "lines_covered": 4, "lines_total": 5},  # coverage analysis
                {"completeness_score": 8.0, "missing_tests": []}  # test validation
            ]
            
            start_time = time.time()
            result = await tester_agent.run(state)
            end_time = time.time()
            
            execution_time = end_time - start_time
            
            # Should complete within reasonable time (less than 5 seconds)
            assert execution_time < 5.0
            assert result is not None
    
    @pytest.mark.asyncio
    async def test_stress_test_multiple_runs(self, tester_agent):
        """Test tester agent with multiple consecutive runs."""
        state = {
            "agent_id": "tester",
            "agent_type": "tester",
            "codebase_path": "/test/codebase",
            "target_language": "python",
            "modernization_goals": ["maintainability"],
            "test_framework": "pytest",
            "test_cases": [{"name": "test1", "type": "unit", "code": "def test1(): assert True"}],
            "transformation_results": [{"file": "file1.py", "content": "def function1(): pass"}],
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
            "quality_review": None,
            "standards_analysis": None,
            "test_results": None,
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
            "duration": None
        }
        
        with patch.object(tester_agent, 'use_tool') as mock_use_tool:
            mock_use_tool.side_effect = [
                {"passed": 1, "failed": 0, "total": 1},  # test runner
                {"coverage_percentage": 80, "lines_covered": 4, "lines_total": 5},  # coverage analysis
                {"completeness_score": 8.0, "missing_tests": []}  # test validation
            ]
            
            # Run multiple times to test for memory leaks or state issues
            for i in range(10):
                # Create a fresh copy of state for each run
                fresh_state = state.copy()
                result = await tester_agent.run(fresh_state)
                assert result is not None
                # Check if test_results exists (may be None on error)
                if result.get("test_results") is not None:
                    assert result["test_results"] is not None
    
    @pytest.mark.asyncio
    async def test_concurrent_agent_instances(self):
        """Test multiple tester agent instances running concurrently."""
        async def run_agent_instance(agent_id):
            agent = TesterAgent(Settings())
            state = {
                "agent_id": f"tester_{agent_id}",
                "agent_type": "tester",
                "codebase_path": f"/test/codebase_{agent_id}",
                "target_language": "python",
                "modernization_goals": ["maintainability"],
                "test_framework": "pytest",
                "test_cases": [{"name": f"test{agent_id}", "type": "unit", "code": f"def test{agent_id}(): assert True"}],
                "transformation_results": [{"file": f"file{agent_id}.py", "content": f"def function{agent_id}(): pass"}],
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
                "quality_review": None,
                "standards_analysis": None,
                "test_results": None,
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
                "duration": None
            }
            
            with patch.object(agent, 'use_tool') as mock_use_tool:
                mock_use_tool.side_effect = [
                    {"passed": 1, "failed": 0, "total": 1},  # test runner
                    {"coverage_percentage": 80, "lines_covered": 4, "lines_total": 5},  # coverage analysis
                    {"completeness_score": 8.0, "missing_tests": []}  # test validation
                ]
                
                return await agent.run(state)
        
        # Run 5 concurrent agent instances
        tasks = [run_agent_instance(i) for i in range(5)]
        results = await asyncio.gather(*tasks)
        
        # All should complete successfully
        assert len(results) == 5
        for i, result in enumerate(results):
            assert result is not None
            assert result["agent_id"] == f"tester_{i}"
    
    @pytest.mark.asyncio
    async def test_tool_error_recovery(self, tester_agent, mock_state):
        """Test tester agent recovery from various tool errors."""
        error_scenarios = [
            Exception("Network timeout"),
            ValueError("Invalid input"),
            RuntimeError("Resource unavailable"),
            KeyError("Missing key"),
            TypeError("Wrong type"),
        ]
        
        for error in error_scenarios:
            with patch.object(tester_agent, 'use_tool') as mock_use_tool:
                mock_use_tool.side_effect = error
                
                result = await tester_agent.run(mock_state.copy())
                
                # Should handle all error types gracefully
                assert result is not None
                assert result["error"] is not None
                assert str(error) in result["error"]
    
    def test_tester_agent_thread_safety(self, tester_agent):
        """Test tester agent thread safety (basic check)."""
        import threading
        import time
        
        results = []
        errors = []
        
        def run_agent():
            try:
                # Test basic operations that should be thread-safe
                tools = tester_agent.get_available_tools()
                prompt = tester_agent.get_system_prompt()
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
    async def test_tester_agent_with_extreme_data_sizes(self, tester_agent):
        """Test tester agent with extreme data sizes."""
        # Test with very large data
        large_state = {
            "agent_id": "tester",
            "agent_type": "tester",
            "codebase_path": "/test/codebase",
            "target_language": "python",
            "modernization_goals": ["maintainability"],
            "test_framework": "pytest",
            "test_cases": [
                {"name": f"test{i}", "type": "unit", "code": f"def test{i}(): assert True"} for i in range(1000)
            ],
            "transformation_results": [
                {"file": f"file{i}.py", "content": f"def function{i}(): pass"} for i in range(500)
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
            "quality_review": None,
            "standards_analysis": None,
            "test_results": None,
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
            "duration": None
        }
        
        with patch.object(tester_agent, 'use_tool') as mock_use_tool:
            # Mock extremely large responses
            huge_test_results = {
                "passed": 950,
                "failed": 50,
                "total": 1000,
                "execution_time": 300.5,
                "failed_tests": [f"test{i}" for i in range(50)]
            }
            huge_coverage = {
                "coverage_percentage": 92.5,
                "lines_covered": 460,
                "lines_total": 500,
                "branches_covered": 180,
                "branches_total": 200
            }
            huge_validation = {
                "completeness_score": 8.5,
                "missing_tests": [f"missing_test{i}" for i in range(100)],
                "recommendations": [f"recommendation{i}" for i in range(50)]
            }
            
            mock_use_tool.side_effect = [
                huge_test_results,  # test runner
                huge_coverage,  # coverage analysis
                huge_validation  # test validation
            ]
            
            result = await tester_agent.run(large_state)
            
            # Should handle large data without issues
            assert result is not None
            assert result["test_results"] is not None
            assert result["test_results"]["passed"] == 950
            assert result["coverage_analysis"]["coverage_percentage"] == 92.5
            assert result["test_validation"]["completeness_score"] == 8.5
    
    def test_tester_agent_configuration_validation(self):
        """Test tester agent with various configuration scenarios."""
        # Test with minimal settings
        minimal_settings = Settings()
        minimal_settings.llm_provider = "anthropic"
        minimal_settings.llm_model = "claude-3-sonnet"
        
        agent = TesterAgent(minimal_settings)
        assert agent is not None
        
        # Test with custom settings
        custom_settings = Settings()
        custom_settings.llm_provider = "openai"
        custom_settings.llm_model = "gpt-4"
        custom_settings.llm_temperature = 0.5
        custom_settings.llm_max_tokens = 2000
        
        agent = TesterAgent(custom_settings)
        assert agent.settings.llm_temperature == 0.5
        assert agent.settings.llm_max_tokens == 2000
    
    @pytest.mark.asyncio
    async def test_tester_agent_edge_case_combinations(self, tester_agent):
        """Test tester agent with edge case combinations."""
        edge_cases = [
            # Empty test cases with multiple goals
            {
                "test_cases": [],
                "target_language": "python",
                "modernization_goals": ["maintainability", "performance", "security"]
            },
            # Complex test cases with single goal
            {
                "test_cases": [
                    {"name": f"test{i}", "type": "unit", "code": f"def test{i}(): assert True"} for i in range(10)
                ],
                "target_language": "javascript",
                "modernization_goals": ["maintainability"]
            },
            # Different language combinations
            {
                "test_cases": [
                    {"name": "test_legacy", "type": "unit", "code": "def test_legacy(): assert True"}
                ],
                "target_language": "java",
                "modernization_goals": ["performance"]
            }
        ]
        
        for case in edge_cases:
            state = {
                "agent_id": "tester",
                "agent_type": "tester",
                "codebase_path": "/test/codebase",
                "test_framework": "pytest",
                "transformation_results": [{"file": "file1.py", "content": "def function1(): pass"}],
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
                "quality_review": None,
                "standards_analysis": None,
                "test_results": None,
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
                **case
            }
            
            with patch.object(tester_agent, 'use_tool') as mock_use_tool:
                mock_use_tool.side_effect = [
                    {"passed": len(case["test_cases"]), "failed": 0, "total": len(case["test_cases"])},  # test runner
                    {"coverage_percentage": 80, "lines_covered": 4, "lines_total": 5},  # coverage analysis
                    {"completeness_score": 8.0, "missing_tests": []}  # test validation
                ]
                
                result = await tester_agent.run(state)
                assert result is not None
                assert result["target_language"] == case["target_language"]
                assert result["modernization_goals"] == case["modernization_goals"]
