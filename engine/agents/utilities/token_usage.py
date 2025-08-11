"""
Tracks token costs for optimization and monitoring.
"""

import json
import logging
import time
from typing import Dict, List, Optional, Any, Union
from dataclasses import dataclass, field
from datetime import datetime, timedelta
from pathlib import Path

logger = logging.getLogger(__name__)

@dataclass
class TokenUsage:
    """Represents a single token usage event."""
    timestamp: datetime
    model: str
    provider: str
    input_tokens: int
    output_tokens: int
    total_tokens: int
    cost_usd: float
    operation: str
    agent_name: Optional[str] = None
    task_id: Optional[str] = None
    metadata: Dict[str, Any] = field(default_factory=dict)

@dataclass
class CostSummary:
    """Summary of token costs over a period."""
    period_start: datetime
    period_end: datetime
    total_requests: int
    total_input_tokens: int
    total_output_tokens: int
    total_tokens: int
    total_cost_usd: float
    average_cost_per_request: float
    cost_by_model: Dict[str, float]
    cost_by_agent: Dict[str, float]
    cost_by_operation: Dict[str, float]

class TokenUsageTracker:
    """
    Tracks token usage and costs for LLM operations.
    
    Provides methods for monitoring usage, calculating costs,
    and generating reports for optimization.
    """
    
    def __init__(self, storage_file: Optional[str] = None):
        self.storage_file = storage_file or "token_usage.json"
        self.usage_records: List[TokenUsage] = []
        self.cost_rates = self._get_default_cost_rates()
        
        # Load existing usage data
        self._load_usage_data()
    
    def _get_default_cost_rates(self) -> Dict[str, Dict[str, float]]:
        """Get default cost rates per 1K tokens for different models."""
        return {
            'openai': {
                'gpt-4': 0.03,  # Input
                'gpt-4-turbo': 0.01,
                'gpt-3.5-turbo': 0.001,
                'gpt-3.5-turbo-16k': 0.002,
                'text-davinci-003': 0.02,
                'text-curie-001': 0.002,
                'text-babbage-001': 0.0005,
                'text-ada-001': 0.0004
            },
            'anthropic': {
                'claude-3-opus': 0.015,
                'claude-3-sonnet': 0.003,
                'claude-3-haiku': 0.00025,
                'claude-2.1': 0.008,
                'claude-2.0': 0.008,
                'claude-instant-1': 0.00163
            },
            'google': {
                'gemini-pro': 0.0005,
                'gemini-pro-vision': 0.0005
            }
        }
    
    def _load_usage_data(self):
        """Load usage data from storage file."""
        try:
            if Path(self.storage_file).exists():
                with open(self.storage_file, 'r', encoding='utf-8') as f:
                    data = json.load(f)
                    
                    # Convert stored data back to TokenUsage objects
                    for record_data in data.get('usage_records', []):
                        usage = TokenUsage(
                            timestamp=datetime.fromisoformat(record_data['timestamp']),
                            model=record_data['model'],
                            provider=record_data['provider'],
                            input_tokens=record_data['input_tokens'],
                            output_tokens=record_data['output_tokens'],
                            total_tokens=record_data['total_tokens'],
                            cost_usd=record_data['cost_usd'],
                            operation=record_data['operation'],
                            agent_name=record_data.get('agent_name'),
                            task_id=record_data.get('task_id'),
                            metadata=record_data.get('metadata', {})
                        )
                        self.usage_records.append(usage)
                    
                    logger.info(f"Loaded {len(self.usage_records)} usage records")
                    
        except Exception as e:
            logger.error(f"Failed to load usage data: {e}")
    
    def _save_usage_data(self):
        """Save usage data to storage file."""
        try:
            # Convert TokenUsage objects to dictionaries
            data = {
                'usage_records': [
                    {
                        'timestamp': record.timestamp.isoformat(),
                        'model': record.model,
                        'provider': record.provider,
                        'input_tokens': record.input_tokens,
                        'output_tokens': record.output_tokens,
                        'total_tokens': record.total_tokens,
                        'cost_usd': record.cost_usd,
                        'operation': record.operation,
                        'agent_name': record.agent_name,
                        'task_id': record.task_id,
                        'metadata': record.metadata
                    }
                    for record in self.usage_records
                ]
            }
            
            with open(self.storage_file, 'w', encoding='utf-8') as f:
                json.dump(data, f, indent=2, default=str)
                
        except Exception as e:
            logger.error(f"Failed to save usage data: {e}")
    
    def record_usage(
        self,
        model: str,
        provider: str,
        input_tokens: int,
        output_tokens: int,
        operation: str,
        agent_name: Optional[str] = None,
        task_id: Optional[str] = None,
        metadata: Optional[Dict[str, Any]] = None
    ) -> TokenUsage:
        """
        Record a token usage event.
        
        Args:
            model: Model name
            provider: Provider name (openai, anthropic, etc.)
            input_tokens: Number of input tokens
            output_tokens: Number of output tokens
            operation: Type of operation
            agent_name: Name of the agent
            task_id: Task identifier
            metadata: Additional metadata
            
        Returns:
            TokenUsage object for the recorded event
        """
        total_tokens = input_tokens + output_tokens
        cost_usd = self._calculate_cost(provider, model, input_tokens, output_tokens)
        
        usage = TokenUsage(
            timestamp=datetime.now(),
            model=model,
            provider=provider,
            input_tokens=input_tokens,
            output_tokens=output_tokens,
            total_tokens=total_tokens,
            cost_usd=cost_usd,
            operation=operation,
            agent_name=agent_name,
            task_id=task_id,
            metadata=metadata or {}
        )
        
        self.usage_records.append(usage)
        self._save_usage_data()
        
        logger.debug(f"Recorded usage: {model} ({total_tokens} tokens, ${cost_usd:.4f})")
        return usage
    
    def _calculate_cost(self, provider: str, model: str, input_tokens: int, output_tokens: int) -> float:
        """Calculate cost for token usage."""
        provider_rates = self.cost_rates.get(provider, {})
        model_rate = provider_rates.get(model, 0.01)  # Default rate
        
        # Calculate cost (rate is per 1K tokens)
        input_cost = (input_tokens / 1000) * model_rate
        output_cost = (output_tokens / 1000) * model_rate
        
        return input_cost + output_cost
    
    def get_usage_summary(
        self,
        start_time: Optional[datetime] = None,
        end_time: Optional[datetime] = None,
        agent_name: Optional[str] = None,
        operation: Optional[str] = None
    ) -> CostSummary:
        """
        Get a summary of token usage for a specific period.
        
        Args:
            start_time: Start of period (defaults to 24 hours ago)
            end_time: End of period (defaults to now)
            agent_name: Filter by agent name
            operation: Filter by operation type
            
        Returns:
            CostSummary object
        """
        if start_time is None:
            start_time = datetime.now() - timedelta(days=1)
        if end_time is None:
            end_time = datetime.now()
        
        # Filter records
        filtered_records = [
            record for record in self.usage_records
            if start_time <= record.timestamp <= end_time
            and (agent_name is None or record.agent_name == agent_name)
            and (operation is None or record.operation == operation)
        ]
        
        if not filtered_records:
            return CostSummary(
                period_start=start_time,
                period_end=end_time,
                total_requests=0,
                total_input_tokens=0,
                total_output_tokens=0,
                total_tokens=0,
                total_cost_usd=0.0,
                average_cost_per_request=0.0,
                cost_by_model={},
                cost_by_agent={},
                cost_by_operation={}
            )
        
        # Calculate totals
        total_requests = len(filtered_records)
        total_input_tokens = sum(r.input_tokens for r in filtered_records)
        total_output_tokens = sum(r.output_tokens for r in filtered_records)
        total_tokens = sum(r.total_tokens for r in filtered_records)
        total_cost_usd = sum(r.cost_usd for r in filtered_records)
        
        # Calculate cost breakdowns
        cost_by_model = {}
        cost_by_agent = {}
        cost_by_operation = {}
        
        for record in filtered_records:
            # By model
            model_key = f"{record.provider}/{record.model}"
            cost_by_model[model_key] = cost_by_model.get(model_key, 0) + record.cost_usd
            
            # By agent
            if record.agent_name:
                cost_by_agent[record.agent_name] = cost_by_agent.get(record.agent_name, 0) + record.cost_usd
            
            # By operation
            cost_by_operation[record.operation] = cost_by_operation.get(record.operation, 0) + record.cost_usd
        
        return CostSummary(
            period_start=start_time,
            period_end=end_time,
            total_requests=total_requests,
            total_input_tokens=total_input_tokens,
            total_output_tokens=total_output_tokens,
            total_tokens=total_tokens,
            total_cost_usd=total_cost_usd,
            average_cost_per_request=total_cost_usd / total_requests if total_requests > 0 else 0.0,
            cost_by_model=cost_by_model,
            cost_by_agent=cost_by_agent,
            cost_by_operation=cost_by_operation
        )
    
    def get_daily_usage(self, days: int = 7) -> List[CostSummary]:
        """Get daily usage summaries for the specified number of days."""
        summaries = []
        end_time = datetime.now()
        
        for i in range(days):
            day_start = end_time - timedelta(days=i+1)
            day_end = end_time - timedelta(days=i)
            summary = self.get_usage_summary(day_start, day_end)
            summaries.append(summary)
        
        return list(reversed(summaries))  # Return in chronological order
    
    def get_agent_performance(self, agent_name: str, days: int = 7) -> Dict[str, Any]:
        """Get performance metrics for a specific agent."""
        end_time = datetime.now()
        start_time = end_time - timedelta(days=days)
        
        agent_records = [
            record for record in self.usage_records
            if record.agent_name == agent_name
            and start_time <= record.timestamp <= end_time
        ]
        
        if not agent_records:
            return {
                'agent_name': agent_name,
                'total_requests': 0,
                'total_cost': 0.0,
                'average_tokens_per_request': 0,
                'operations': {},
                'daily_usage': []
            }
        
        # Calculate metrics
        total_requests = len(agent_records)
        total_cost = sum(r.cost_usd for r in agent_records)
        total_tokens = sum(r.total_tokens for r in agent_records)
        average_tokens = total_tokens / total_requests if total_requests > 0 else 0
        
        # Operations breakdown
        operations = {}
        for record in agent_records:
            operations[record.operation] = operations.get(record.operation, 0) + 1
        
        # Daily usage
        daily_usage = []
        for i in range(days):
            day_start = end_time - timedelta(days=i+1)
            day_end = end_time - timedelta(days=i)
            day_records = [
                r for r in agent_records
                if day_start <= r.timestamp <= day_end
            ]
            daily_usage.append({
                'date': day_start.date().isoformat(),
                'requests': len(day_records),
                'cost': sum(r.cost_usd for r in day_records),
                'tokens': sum(r.total_tokens for r in day_records)
            })
        
        return {
            'agent_name': agent_name,
            'total_requests': total_requests,
            'total_cost': total_cost,
            'average_tokens_per_request': average_tokens,
            'operations': operations,
            'daily_usage': list(reversed(daily_usage))
        }
    
    def get_cost_optimization_suggestions(self) -> List[Dict[str, Any]]:
        """Get suggestions for cost optimization."""
        suggestions = []
        
        # Get recent usage (last 7 days)
        recent_summary = self.get_usage_summary(
            start_time=datetime.now() - timedelta(days=7)
        )
        
        if recent_summary.total_requests == 0:
            return suggestions
        
        # Check for expensive models
        for model, cost in recent_summary.cost_by_model.items():
            if cost > 1.0:  # More than $1
                suggestions.append({
                    'type': 'expensive_model',
                    'model': model,
                    'cost': cost,
                    'suggestion': f"Consider using a cheaper model for non-critical tasks. {model} cost: ${cost:.2f}"
                })
        
        # Check for high token usage
        if recent_summary.average_cost_per_request > 0.1:  # More than 10 cents per request
            suggestions.append({
                'type': 'high_cost_per_request',
                'average_cost': recent_summary.average_cost_per_request,
                'suggestion': f"Average cost per request is ${recent_summary.average_cost_per_request:.4f}. Consider optimizing prompts to reduce token usage."
            })
        
        # Check for inefficient agents
        for agent, cost in recent_summary.cost_by_agent.items():
            agent_records = [
                r for r in self.usage_records
                if r.agent_name == agent
                and r.timestamp >= datetime.now() - timedelta(days=7)
            ]
            
            if agent_records:
                avg_tokens = sum(r.total_tokens for r in agent_records) / len(agent_records)
                if avg_tokens > 2000:  # More than 2000 tokens per request
                    suggestions.append({
                        'type': 'high_token_usage',
                        'agent': agent,
                        'average_tokens': avg_tokens,
                        'suggestion': f"Agent {agent} uses {avg_tokens:.0f} tokens on average. Consider optimizing prompts or using more efficient models."
                    })
        
        return suggestions
    
    def export_usage_report(self, file_path: str, format: str = "json"):
        """Export usage data to a file."""
        try:
            if format.lower() == "json":
                with open(file_path, 'w', encoding='utf-8') as f:
                    json.dump({
                        'usage_records': [
                            {
                                'timestamp': record.timestamp.isoformat(),
                                'model': record.model,
                                'provider': record.provider,
                                'input_tokens': record.input_tokens,
                                'output_tokens': record.output_tokens,
                                'total_tokens': record.total_tokens,
                                'cost_usd': record.cost_usd,
                                'operation': record.operation,
                                'agent_name': record.agent_name,
                                'task_id': record.task_id,
                                'metadata': record.metadata
                            }
                            for record in self.usage_records
                        ]
                    }, f, indent=2, default=str)
            
            elif format.lower() == "csv":
                import csv
                with open(file_path, 'w', newline='', encoding='utf-8') as f:
                    writer = csv.writer(f)
                    writer.writerow([
                        'timestamp', 'model', 'provider', 'input_tokens',
                        'output_tokens', 'total_tokens', 'cost_usd',
                        'operation', 'agent_name', 'task_id'
                    ])
                    
                    for record in self.usage_records:
                        writer.writerow([
                            record.timestamp.isoformat(),
                            record.model,
                            record.provider,
                            record.input_tokens,
                            record.output_tokens,
                            record.total_tokens,
                            record.cost_usd,
                            record.operation,
                            record.agent_name,
                            record.task_id
                        ])
            
            logger.info(f"Usage report exported to {file_path}")
            
        except Exception as e:
            logger.error(f"Failed to export usage report: {e}")
    
    def clear_old_records(self, days: int = 30):
        """Clear usage records older than specified days."""
        cutoff_time = datetime.now() - timedelta(days=days)
        original_count = len(self.usage_records)
        
        self.usage_records = [
            record for record in self.usage_records
            if record.timestamp >= cutoff_time
        ]
        
        removed_count = original_count - len(self.usage_records)
        if removed_count > 0:
            self._save_usage_data()
            logger.info(f"Cleared {removed_count} old usage records")
    
    def get_statistics(self) -> Dict[str, Any]:
        """Get overall statistics about token usage."""
        if not self.usage_records:
            return {
                'total_records': 0,
                'total_cost': 0.0,
                'total_tokens': 0,
                'average_cost_per_request': 0.0,
                'most_used_model': None,
                'most_expensive_agent': None
            }
        
        total_records = len(self.usage_records)
        total_cost = sum(r.cost_usd for r in self.usage_records)
        total_tokens = sum(r.total_tokens for r in self.usage_records)
        average_cost = total_cost / total_records if total_records > 0 else 0.0
        
        # Most used model
        model_usage = {}
        for record in self.usage_records:
            model_key = f"{record.provider}/{record.model}"
            model_usage[model_key] = model_usage.get(model_key, 0) + record.total_tokens
        
        most_used_model = max(model_usage.items(), key=lambda x: x[1])[0] if model_usage else None
        
        # Most expensive agent
        agent_costs = {}
        for record in self.usage_records:
            if record.agent_name:
                agent_costs[record.agent_name] = agent_costs.get(record.agent_name, 0) + record.cost_usd
        
        most_expensive_agent = max(agent_costs.items(), key=lambda x: x[1])[0] if agent_costs else None
        
        return {
            'total_records': total_records,
            'total_cost': total_cost,
            'total_tokens': total_tokens,
            'average_cost_per_request': average_cost,
            'most_used_model': most_used_model,
            'most_expensive_agent': most_expensive_agent,
            'date_range': {
                'earliest': min(r.timestamp for r in self.usage_records).isoformat(),
                'latest': max(r.timestamp for r in self.usage_records).isoformat()
            }
        } 