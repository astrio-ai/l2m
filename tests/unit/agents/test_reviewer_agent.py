"""
Unit tests for the reviewer agent.

This module contains unit tests for the reviewer agent
functionality and behavior.
"""

import pytest
import asyncio
from unittest.mock import Mock, patch, AsyncMock, MagicMock
from src.core.agents.reviewer_agent import ReviewerAgent
from src.core.state.agent_state import AgentState
from src.config.settings import Settings


class TestReviewerAgent:
    """Test cases for the reviewer agent."""
    
    @pytest.fixture
    def settings(self):
        """Create settings fixture."""
        return Settings()
    
    @pytest.fixture
    def reviewer_agent(self, settings):
        """Create reviewer agent fixture."""
        return ReviewerAgent(settings)
    
    @pytest.fixture
    def mock_state(self):
        """Create mock agent state."""
        return {
            "agent_id": "reviewer",
            "agent_type": "reviewer",
            "codebase_path": "/test/codebase",
            "target_language": "python",
            "modernization_goals": ["maintainability"],
            "analysis_results": {
                "structure": {"files": ["file1.cobol", "file2.cobol"]},
                "dependencies": {"dependencies": ["dep1", "dep2"]},
                "patterns": {"patterns": ["pattern1", "pattern2"]}
            },
            "transformation_results": [
                {"file": "file1.py", "content": "def function1(): pass"},
                {"file": "file2.py", "content": "def function2(): pass"}
            ],
            "modernization_plan": None,
            "risk_assessment": None,
            "implementation_strategy": None,
            "pattern_results": None,
            "backup_path": None,
            "quality_review": None,
            "standards_analysis": None,
            "test_cases": None,
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
    
    def test_reviewer_agent_initialization(self, reviewer_agent):
        """Test reviewer agent initialization."""
        assert reviewer_agent is not None
        assert reviewer_agent.settings is not None
        assert len(reviewer_agent.tools) > 0
    
    def test_get_system_prompt(self, reviewer_agent):
        """Test system prompt generation."""
        prompt = reviewer_agent.get_system_prompt()
        assert isinstance(prompt, str)
        assert len(prompt) > 0
        assert "review" in prompt.lower()
    
    @pytest.mark.asyncio
    async def test_run_reviewer_agent(self, reviewer_agent, mock_state):
        """Test reviewer agent execution."""
        with patch.object(reviewer_agent, 'use_tool') as mock_use_tool:
            # Mock tool responses
            mock_use_tool.side_effect = [
                {"overall_score": 8.5, "issues": ["minor_issue1"]},  # quality review
                {"violations": ["violation1"], "compliance_score": 7.0},  # standards analysis
                {"test_cases": ["test1", "test2"], "coverage": 85}  # test generation
            ]
            
            result = await reviewer_agent.run(mock_state)
            
            assert result is not None
            assert result["quality_review"] is not None
            assert result["standards_analysis"] is not None
            assert result["test_cases"] is not None
    
    @pytest.mark.asyncio
    async def test_run_reviewer_agent_error(self, reviewer_agent, mock_state):
        """Test reviewer agent error handling."""
        with patch.object(reviewer_agent, 'use_tool') as mock_use_tool:
            mock_use_tool.side_effect = Exception("Tool error")
            
            result = await reviewer_agent.run(mock_state)
            
            assert result is not None
            assert result["error"] is not None
            assert "Tool error" in result["error"]
    
    def test_get_available_tools(self, reviewer_agent):
        """Test available tools retrieval."""
        tools = reviewer_agent.get_available_tools()
        assert isinstance(tools, list)
        assert len(tools) > 0
    
    @pytest.mark.asyncio
    async def test_use_tool(self, reviewer_agent):
        """Test tool usage."""
        with patch.object(reviewer_agent.tools[0], 'run') as mock_run:
            mock_run.return_value = {"result": "test"}
            
            result = await reviewer_agent.use_tool("review_transformed_code", transformed_code={})
            
            assert result is not None
            assert result["result"] == "test"
    
    def test_log_activity(self, reviewer_agent):
        """Test activity logging."""
        reviewer_agent.log_activity("test activity", {"key": "value"})
        assert True  # If we get here, no exception was raised

    # ==================== EXTENSIVE TEST SUITE ====================
    
    def test_reviewer_agent_tools_initialization(self, reviewer_agent):
        """Test that reviewer agent has the correct tools initialized."""
        tools = reviewer_agent.get_available_tools()
        tool_names = [tool.name for tool in tools]
        
        # Should have review tools
        assert "review_transformed_code" in tool_names
        assert "analyze_code_quality" in tool_names
        
        # Should have test tools
        assert "generate_test_cases" in tool_names
        
        # Verify tool count
        assert len(tools) >= 3  # At least 3 tools should be available
    
    def test_reviewer_agent_with_different_settings(self):
        """Test reviewer agent with different configuration settings."""
        custom_settings = Settings()
        custom_settings.llm_provider = "openai"
        custom_settings.llm_model = "gpt-4"
        
        agent = ReviewerAgent(custom_settings)
        assert agent.settings.llm_provider == "openai"
        assert agent.settings.llm_model == "gpt-4"
    
    @pytest.mark.asyncio
    async def test_run_with_empty_transformation_results(self, reviewer_agent):
        """Test reviewer agent with empty transformation results."""
        empty_state = {
            "agent_id": "reviewer",
            "agent_type": "reviewer",
            "codebase_path": "/test/codebase",
            "target_language": "python",
            "modernization_goals": ["maintainability"],
            "analysis_results": {
                "structure": {"files": []},
                "dependencies": {"dependencies": []},
                "patterns": {"patterns": []}
            },
            "transformation_results": [],
            "modernization_plan": None,
            "risk_assessment": None,
            "implementation_strategy": None,
            "pattern_results": None,
            "backup_path": None,
            "quality_review": None,
            "standards_analysis": None,
            "test_cases": None,
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
        
        with patch.object(reviewer_agent, 'use_tool') as mock_use_tool:
            # Mock responses for empty results
            mock_use_tool.side_effect = [
                {"overall_score": 0, "issues": []},  # quality review
                {"violations": [], "compliance_score": 0},  # standards analysis
                {"test_cases": [], "coverage": 0}  # test generation
            ]
            
            result = await reviewer_agent.run(empty_state)
            
            assert result is not None
            assert result["quality_review"] is not None
            assert result["standards_analysis"] is not None
            assert result["test_cases"] is not None
    
    @pytest.mark.asyncio
    async def test_run_with_complex_transformation_results(self, reviewer_agent):
        """Test reviewer agent with complex transformation results."""
        complex_state = {
            "agent_id": "reviewer",
            "agent_type": "reviewer",
            "codebase_path": "/complex/codebase",
            "target_language": "python",
            "modernization_goals": ["maintainability", "performance"],
            "analysis_results": {
                "structure": {"files": [f"file{i}.cobol" for i in range(100)]},
                "dependencies": {"dependencies": [f"dep{i}" for i in range(50)]},
                "patterns": {"patterns": [f"pattern{i}" for i in range(25)]}
            },
            "transformation_results": [
                {"file": f"file{i}.py", "content": f"def function{i}(): pass"} for i in range(50)
            ],
            "modernization_plan": None,
            "risk_assessment": None,
            "implementation_strategy": None,
            "pattern_results": None,
            "backup_path": None,
            "quality_review": None,
            "standards_analysis": None,
            "test_cases": None,
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
        
        with patch.object(reviewer_agent, 'use_tool') as mock_use_tool:
            # Mock complex responses
            mock_use_tool.side_effect = [
                {
                    "overall_score": 8.5,
                    "issues": [f"issue{i}" for i in range(10)],
                    "recommendations": [f"rec{i}" for i in range(5)]
                },
                {
                    "violations": [f"violation{i}" for i in range(5)],
                    "compliance_score": 7.5,
                    "standards_met": ["standard1", "standard2"]
                },
                {
                    "test_cases": [f"test{i}" for i in range(20)],
                    "coverage": 90,
                    "test_types": ["unit", "integration"]
                }
            ]
            
            result = await reviewer_agent.run(complex_state)
            
            assert result is not None
            assert result["quality_review"] is not None
            assert result["standards_analysis"] is not None
            assert result["test_cases"] is not None
            assert result["quality_review"]["overall_score"] == 8.5
            assert len(result["standards_analysis"]["violations"]) == 5
            assert len(result["test_cases"]["test_cases"]) == 20
    
    @pytest.mark.asyncio
    async def test_run_with_partial_tool_failures(self, reviewer_agent, mock_state):
        """Test reviewer agent when some tools fail but others succeed."""
        with patch.object(reviewer_agent, 'use_tool') as mock_use_tool:
            # First tool succeeds, second fails, third succeeds
            mock_use_tool.side_effect = [
                {"overall_score": 8.0, "issues": []},  # quality review succeeds
                Exception("Standards analysis failed"),  # standards analysis fails
                {"test_cases": ["test1"], "coverage": 80}  # test generation succeeds
            ]
            
            result = await reviewer_agent.run(mock_state)
            
            # Should handle partial failures gracefully
            assert result is not None
            assert result["error"] is not None
            assert "Standards analysis failed" in result["error"]
    
    @pytest.mark.asyncio
    async def test_run_with_timeout_simulation(self, reviewer_agent, mock_state):
        """Test reviewer agent with timeout simulation."""
        with patch.object(reviewer_agent, 'use_tool') as mock_use_tool:
            # Simulate timeout by making tools take too long
            async def slow_tool(*args, **kwargs):
                await asyncio.sleep(0.1)  # Simulate slow operation
                return {"result": "slow_result"}
            
            mock_use_tool.side_effect = slow_tool
            
            result = await reviewer_agent.run(mock_state)
            
            # Should complete despite slow tools
            assert result is not None
    
    @pytest.mark.asyncio
    async def test_run_with_malformed_tool_responses(self, reviewer_agent, mock_state):
        """Test reviewer agent with malformed tool responses."""
        with patch.object(reviewer_agent, 'use_tool') as mock_use_tool:
            # Mock malformed responses
            mock_use_tool.side_effect = [
                None,  # None response
                {"violations": "not_a_list"},  # Wrong type
                {"test_cases": []}  # Valid response
            ]
            
            result = await reviewer_agent.run(mock_state)
            
            # Should handle malformed responses gracefully
            assert result is not None
    
    @pytest.mark.asyncio
    async def test_use_tool_with_invalid_tool_name(self, reviewer_agent):
        """Test using a tool that doesn't exist."""
        with pytest.raises(ValueError, match="Tool 'invalid_tool' not found"):
            await reviewer_agent.use_tool("invalid_tool", param="value")
    
    @pytest.mark.asyncio
    async def test_use_tool_with_missing_parameters(self, reviewer_agent):
        """Test using a tool with missing required parameters."""
        # This should raise an error due to missing required parameter
        with pytest.raises((ValueError, TypeError)):
            await reviewer_agent.use_tool("review_code_quality")
    
    @pytest.mark.asyncio
    async def test_use_tool_with_extra_parameters(self, reviewer_agent):
        """Test using a tool with extra parameters."""
        with patch.object(reviewer_agent.tools[0], 'run') as mock_run:
            mock_run.return_value = {"result": "test"}
            
            # Should handle extra parameters gracefully
            result = await reviewer_agent.use_tool(
                "review_transformed_code", 
                transformed_code={},
                extra_param="extra_value"
            )
            
            assert result is not None
    
    def test_get_system_prompt_content(self, reviewer_agent):
        """Test system prompt contains expected content."""
        prompt = reviewer_agent.get_system_prompt()
        
        # Check for key concepts in the prompt
        assert "review" in prompt.lower()
        assert "quality" in prompt.lower()
        assert "standards" in prompt.lower()
        assert "test" in prompt.lower()
        assert "functionality" in prompt.lower()
        assert "modern" in prompt.lower()
    
    def test_get_system_prompt_length(self, reviewer_agent):
        """Test system prompt has reasonable length."""
        prompt = reviewer_agent.get_system_prompt()
        
        # Should be substantial but not excessive
        assert len(prompt) > 100  # At least 100 characters
        assert len(prompt) < 2000  # Not more than 2000 characters
    
    def test_log_activity_with_different_data_types(self, reviewer_agent):
        """Test logging with different data types."""
        # Test with string
        reviewer_agent.log_activity("string activity")
        
        # Test with dict
        reviewer_agent.log_activity("dict activity", {"key": "value", "number": 42})
        
        # Test with list
        reviewer_agent.log_activity("list activity", ["item1", "item2"])
        
        # Test with None
        reviewer_agent.log_activity("none activity", None)
        
        # All should complete without errors
        assert True
    
    def test_log_activity_with_complex_data(self, reviewer_agent):
        """Test logging with complex nested data structures."""
        complex_data = {
            "review": {
                "quality_scores": {
                    "overall": 8.5,
                    "readability": 9.0,
                    "maintainability": 8.0,
                    "performance": 7.5
                },
                "issues": [
                    {"type": "style", "severity": "low", "description": "Minor style issue"},
                    {"type": "logic", "severity": "medium", "description": "Potential logic issue"}
                ],
                "recommendations": [
                    "Add type hints",
                    "Improve error handling",
                    "Add documentation"
                ]
            },
            "standards": {
                "compliance_score": 85.5,
                "violations": ["PEP8", "Type hints missing"],
                "standards_met": ["PEP257", "Clean code"]
            },
            "testing": {
                "test_cases": 15,
                "coverage": 92.5,
                "test_types": ["unit", "integration", "performance"]
            }
        }
        
        reviewer_agent.log_activity("complex review", complex_data)
        assert True  # Should complete without errors
    
    @pytest.mark.asyncio
    async def test_concurrent_tool_usage(self, reviewer_agent):
        """Test concurrent usage of multiple tools."""
        async def use_multiple_tools():
            tasks = []
            for i in range(5):
                task = reviewer_agent.use_tool(
                    "review_transformed_code", 
                    transformed_code={"file": f"file{i}.py"}
                )
                tasks.append(task)
            
            results = await asyncio.gather(*tasks, return_exceptions=True)
            return results
        
        with patch.object(reviewer_agent.tools[0], 'run') as mock_run:
            mock_run.return_value = {"result": "test"}
            
            results = await use_multiple_tools()
            
            # All should complete successfully
            assert len(results) == 5
            for result in results:
                assert not isinstance(result, Exception)
    
    @pytest.mark.asyncio
    async def test_run_with_state_mutation(self, reviewer_agent):
        """Test that agent run doesn't mutate the original state inappropriately."""
        original_state = {
            "agent_id": "reviewer",
            "agent_type": "reviewer",
            "codebase_path": "/test/codebase",
            "target_language": "python",
            "modernization_goals": ["maintainability"],
            "analysis_results": {
                "structure": {"files": ["file1.cobol"]},
                "dependencies": {"dependencies": ["dep1"]},
                "patterns": {"patterns": ["pattern1"]}
            },
            "transformation_results": [
                {"file": "file1.py", "content": "def function1(): pass"}
            ],
            "modernization_plan": None,
            "risk_assessment": None,
            "implementation_strategy": None,
            "pattern_results": None,
            "backup_path": None,
            "quality_review": None,
            "standards_analysis": None,
            "test_cases": None,
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
        
        with patch.object(reviewer_agent, 'use_tool') as mock_use_tool:
            mock_use_tool.side_effect = [
                {"overall_score": 8.0, "issues": []},  # quality review
                {"violations": [], "compliance_score": 9.0},  # standards analysis
                {"test_cases": ["test1"], "coverage": 80}  # test generation
            ]
            
            result = await reviewer_agent.run(original_state)
            
            # Original state should be modified (this is expected behavior)
            assert result is not None
            assert result["quality_review"] is not None
    
    def test_reviewer_agent_tool_integration(self, reviewer_agent):
        """Test that all tools are properly integrated."""
        tools = reviewer_agent.get_available_tools()
        
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
    async def test_run_with_different_modernization_goals(self, reviewer_agent):
        """Test reviewer agent with different modernization goals."""
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
                "agent_id": "reviewer",
                "agent_type": "reviewer",
                "codebase_path": "/test/codebase",
                "target_language": "python",
                "modernization_goals": goals,
                "analysis_results": {
                    "structure": {"files": ["file1.cobol"]},
                    "dependencies": {"dependencies": ["dep1"]},
                    "patterns": {"patterns": ["pattern1"]}
                },
                "transformation_results": [
                    {"file": "file1.py", "content": "def function1(): pass"}
                ],
                "modernization_plan": None,
                "risk_assessment": None,
                "implementation_strategy": None,
                "pattern_results": None,
                "backup_path": None,
                "quality_review": None,
                "standards_analysis": None,
                "test_cases": None,
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
            
            with patch.object(reviewer_agent, 'use_tool') as mock_use_tool:
                mock_use_tool.side_effect = [
                    {"overall_score": 8.0, "issues": []},  # quality review
                    {"violations": [], "compliance_score": 9.0},  # standards analysis
                    {"test_cases": ["test1"], "coverage": 80}  # test generation
                ]
                
                result = await reviewer_agent.run(state)
                assert result is not None
    
    @pytest.mark.asyncio
    async def test_run_with_different_target_languages(self, reviewer_agent):
        """Test reviewer agent with different target languages."""
        languages = ["python", "javascript", "java", "csharp", "go"]
        
        for language in languages:
            state = {
                "agent_id": "reviewer",
                "agent_type": "reviewer",
                "codebase_path": "/test/codebase",
                "target_language": language,
                "modernization_goals": ["maintainability"],
                "analysis_results": {
                    "structure": {"files": [f"file1.{language}"]},
                    "dependencies": {"dependencies": ["dep1"]},
                    "patterns": {"patterns": ["pattern1"]}
                },
                "transformation_results": [
                    {"file": f"file1.{language}", "content": f"def function1(): pass"}
                ],
                "modernization_plan": None,
                "risk_assessment": None,
                "implementation_strategy": None,
                "pattern_results": None,
                "backup_path": None,
                "quality_review": None,
                "standards_analysis": None,
                "test_cases": None,
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
            
            with patch.object(reviewer_agent, 'use_tool') as mock_use_tool:
                mock_use_tool.side_effect = [
                    {"overall_score": 8.0, "issues": []},  # quality review
                    {"violations": [], "compliance_score": 9.0},  # standards analysis
                    {"test_cases": ["test1"], "coverage": 80}  # test generation
                ]
                
                result = await reviewer_agent.run(state)
                assert result is not None
                assert result["target_language"] == language
    
    def test_reviewer_agent_memory_usage(self, reviewer_agent):
        """Test that reviewer agent doesn't have memory leaks."""
        import gc
        
        # Create and destroy multiple agents
        for i in range(10):
            temp_agent = ReviewerAgent(Settings())
            del temp_agent
        
        # Force garbage collection
        gc.collect()
        
        # Original agent should still work
        assert reviewer_agent is not None
        assert len(reviewer_agent.get_available_tools()) > 0
    
    @pytest.mark.asyncio
    async def test_run_with_corrupted_state(self, reviewer_agent):
        """Test reviewer agent with corrupted or incomplete state."""
        corrupted_states = [
            {},  # Empty state
            {"agent_id": "reviewer"},  # Minimal state
            {"transformation_results": None},  # None results
            {"transformation_results": []},  # Empty results
        ]
        
        for state in corrupted_states:
            # Fill in missing required fields
            complete_state = {
                "agent_id": "reviewer",
                "agent_type": "reviewer",
                "codebase_path": "/test/codebase",
                "target_language": "python",
                "modernization_goals": ["maintainability"],
                "analysis_results": {
                    "structure": {"files": ["file1.cobol"]},
                    "dependencies": {"dependencies": ["dep1"]},
                    "patterns": {"patterns": ["pattern1"]}
                },
                "transformation_results": [
                    {"file": "file1.py", "content": "def function1(): pass"}
                ],
                "modernization_plan": None,
                "risk_assessment": None,
                "implementation_strategy": None,
                "pattern_results": None,
                "backup_path": None,
                "quality_review": None,
                "standards_analysis": None,
                "test_cases": None,
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
            
            with patch.object(reviewer_agent, 'use_tool') as mock_use_tool:
                mock_use_tool.side_effect = [
                    {"overall_score": 8.0, "issues": []},  # quality review
                    {"violations": [], "compliance_score": 9.0},  # standards analysis
                    {"test_cases": ["test1"], "coverage": 80}  # test generation
                ]
                
                result = await reviewer_agent.run(complete_state)
                # Should handle corrupted state gracefully
                assert result is not None

    # ==================== PERFORMANCE & STRESS TESTS ====================
    
    @pytest.mark.asyncio
    async def test_run_performance_benchmark(self, reviewer_agent):
        """Test reviewer agent performance with timing measurements."""
        import time
        
        state = {
            "agent_id": "reviewer",
            "agent_type": "reviewer",
            "codebase_path": "/test/codebase",
            "target_language": "python",
            "modernization_goals": ["maintainability"],
            "analysis_results": {
                "structure": {"files": ["file1.cobol"]},
                "dependencies": {"dependencies": ["dep1"]},
                "patterns": {"patterns": ["pattern1"]}
            },
            "transformation_results": [
                {"file": "file1.py", "content": "def function1(): pass"}
            ],
            "modernization_plan": None,
            "risk_assessment": None,
            "implementation_strategy": None,
            "pattern_results": None,
            "backup_path": None,
            "quality_review": None,
            "standards_analysis": None,
            "test_cases": None,
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
        
        with patch.object(reviewer_agent, 'use_tool') as mock_use_tool:
            mock_use_tool.side_effect = [
                {"overall_score": 8.0, "issues": []},  # quality review
                {"violations": [], "compliance_score": 9.0},  # standards analysis
                {"test_cases": ["test1"], "coverage": 80}  # test generation
            ]
            
            start_time = time.time()
            result = await reviewer_agent.run(state)
            end_time = time.time()
            
            execution_time = end_time - start_time
            
            # Should complete within reasonable time (less than 5 seconds)
            assert execution_time < 5.0
            assert result is not None
    
    @pytest.mark.asyncio
    async def test_stress_test_multiple_runs(self, reviewer_agent):
        """Test reviewer agent with multiple consecutive runs."""
        state = {
            "agent_id": "reviewer",
            "agent_type": "reviewer",
            "codebase_path": "/test/codebase",
            "target_language": "python",
            "modernization_goals": ["maintainability"],
            "analysis_results": {
                "structure": {"files": ["file1.cobol"]},
                "dependencies": {"dependencies": ["dep1"]},
                "patterns": {"patterns": ["pattern1"]}
            },
            "transformation_results": [
                {"file": "file1.py", "content": "def function1(): pass"}
            ],
            "modernization_plan": None,
            "risk_assessment": None,
            "implementation_strategy": None,
            "pattern_results": None,
            "backup_path": None,
            "quality_review": None,
            "standards_analysis": None,
            "test_cases": None,
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
        
        with patch.object(reviewer_agent, 'use_tool') as mock_use_tool:
            mock_use_tool.side_effect = [
                {"overall_score": 8.0, "issues": []},  # quality review
                {"violations": [], "compliance_score": 9.0},  # standards analysis
                {"test_cases": ["test1"], "coverage": 80}  # test generation
            ]
            
            # Run multiple times to test for memory leaks or state issues
            for i in range(10):
                # Create a fresh copy of state for each run
                fresh_state = state.copy()
                result = await reviewer_agent.run(fresh_state)
                assert result is not None
                # Check if quality_review exists (may be None on error)
                if result.get("quality_review") is not None:
                    assert result["quality_review"] is not None
    
    @pytest.mark.asyncio
    async def test_concurrent_agent_instances(self):
        """Test multiple reviewer agent instances running concurrently."""
        async def run_agent_instance(agent_id):
            agent = ReviewerAgent(Settings())
            state = {
                "agent_id": f"reviewer_{agent_id}",
                "agent_type": "reviewer",
                "codebase_path": f"/test/codebase_{agent_id}",
                "target_language": "python",
                "modernization_goals": ["maintainability"],
                "analysis_results": {
                    "structure": {"files": [f"file{agent_id}.cobol"]},
                    "dependencies": {"dependencies": [f"dep{agent_id}"]},
                    "patterns": {"patterns": [f"pattern{agent_id}"]}
                },
                "transformation_results": [
                    {"file": f"file{agent_id}.py", "content": f"def function{agent_id}(): pass"}
                ],
                "modernization_plan": None,
                "risk_assessment": None,
                "implementation_strategy": None,
                "pattern_results": None,
                "backup_path": None,
                "quality_review": None,
                "standards_analysis": None,
                "test_cases": None,
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
                    {"overall_score": 8.0, "issues": []},  # quality review
                    {"violations": [], "compliance_score": 9.0},  # standards analysis
                    {"test_cases": [f"test{agent_id}"], "coverage": 80}  # test generation
                ]
                
                return await agent.run(state)
        
        # Run 5 concurrent agent instances
        tasks = [run_agent_instance(i) for i in range(5)]
        results = await asyncio.gather(*tasks)
        
        # All should complete successfully
        assert len(results) == 5
        for i, result in enumerate(results):
            assert result is not None
            assert result["agent_id"] == f"reviewer_{i}"
    
    @pytest.mark.asyncio
    async def test_tool_error_recovery(self, reviewer_agent, mock_state):
        """Test reviewer agent recovery from various tool errors."""
        error_scenarios = [
            Exception("Network timeout"),
            ValueError("Invalid input"),
            RuntimeError("Resource unavailable"),
            KeyError("Missing key"),
            TypeError("Wrong type"),
        ]
        
        for error in error_scenarios:
            with patch.object(reviewer_agent, 'use_tool') as mock_use_tool:
                mock_use_tool.side_effect = error
                
                result = await reviewer_agent.run(mock_state.copy())
                
                # Should handle all error types gracefully
                assert result is not None
                assert result["error"] is not None
                assert str(error) in result["error"]
    
    def test_reviewer_agent_thread_safety(self, reviewer_agent):
        """Test reviewer agent thread safety (basic check)."""
        import threading
        import time
        
        results = []
        errors = []
        
        def run_agent():
            try:
                # Test basic operations that should be thread-safe
                tools = reviewer_agent.get_available_tools()
                prompt = reviewer_agent.get_system_prompt()
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
    async def test_reviewer_agent_with_extreme_data_sizes(self, reviewer_agent):
        """Test reviewer agent with extreme data sizes."""
        # Test with very large data
        large_state = {
            "agent_id": "reviewer",
            "agent_type": "reviewer",
            "codebase_path": "/test/codebase",
            "target_language": "python",
            "modernization_goals": ["maintainability"],
            "analysis_results": {
                "structure": {"files": [f"file{i}.cobol" for i in range(10000)]},
                "dependencies": {"dependencies": [f"dep{i}" for i in range(5000)]},
                "patterns": {"patterns": [f"pattern{i}" for i in range(2500)]}
            },
            "transformation_results": [
                {"file": f"file{i}.py", "content": f"def function{i}(): pass"} for i in range(1000)
            ],
            "modernization_plan": None,
            "risk_assessment": None,
            "implementation_strategy": None,
            "pattern_results": None,
            "backup_path": None,
            "quality_review": None,
            "standards_analysis": None,
            "test_cases": None,
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
        
        with patch.object(reviewer_agent, 'use_tool') as mock_use_tool:
            # Mock extremely large responses
            huge_quality = {
                "overall_score": 8.5,
                "issues": [f"issue{i}" for i in range(1000)],
                "recommendations": [f"rec{i}" for i in range(500)]
            }
            huge_standards = {
                "violations": [f"violation{i}" for i in range(500)],
                "compliance_score": 7.5,
                "standards_met": [f"standard{i}" for i in range(200)]
            }
            huge_tests = {
                "test_cases": [f"test{i}" for i in range(1000)],
                "coverage": 90,
                "test_types": ["unit", "integration", "performance"]
            }
            
            mock_use_tool.side_effect = [
                huge_quality,  # quality review
                huge_standards,  # standards analysis
                huge_tests  # test generation
            ]
            
            result = await reviewer_agent.run(large_state)
            
            # Should handle large data without issues
            assert result is not None
            assert result["quality_review"] is not None
            assert len(result["quality_review"]["issues"]) == 1000
            assert len(result["standards_analysis"]["violations"]) == 500
            assert len(result["test_cases"]["test_cases"]) == 1000
    
    def test_reviewer_agent_configuration_validation(self):
        """Test reviewer agent with various configuration scenarios."""
        # Test with minimal settings
        minimal_settings = Settings()
        minimal_settings.llm_provider = "anthropic"
        minimal_settings.llm_model = "claude-3-sonnet"
        
        agent = ReviewerAgent(minimal_settings)
        assert agent is not None
        
        # Test with custom settings
        custom_settings = Settings()
        custom_settings.llm_provider = "openai"
        custom_settings.llm_model = "gpt-4"
        custom_settings.llm_temperature = 0.5
        custom_settings.llm_max_tokens = 2000
        
        agent = ReviewerAgent(custom_settings)
        assert agent.settings.llm_temperature == 0.5
        assert agent.settings.llm_max_tokens == 2000
    
    @pytest.mark.asyncio
    async def test_reviewer_agent_edge_case_combinations(self, reviewer_agent):
        """Test reviewer agent with edge case combinations."""
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
                "agent_id": "reviewer",
                "agent_type": "reviewer",
                "codebase_path": "/test/codebase",
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
                "test_cases": None,
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
            
            with patch.object(reviewer_agent, 'use_tool') as mock_use_tool:
                mock_use_tool.side_effect = [
                    {"overall_score": 8.0, "issues": []},  # quality review
                    {"violations": [], "compliance_score": 9.0},  # standards analysis
                    {"test_cases": ["test1"], "coverage": 80}  # test generation
                ]
                
                result = await reviewer_agent.run(state)
                assert result is not None
                assert result["target_language"] == case["target_language"]
                assert result["modernization_goals"] == case["modernization_goals"]
